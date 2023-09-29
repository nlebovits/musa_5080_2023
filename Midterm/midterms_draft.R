library(tidyverse)
library(olsrr)
library(sf)
library(caret) # add dummy vars
library(tmap)
library(fastDummies)
library(tidycensus)
library(sfdep)
library(curl)
library(zip)

tmap_mode('view')
options(tigris_use_cache = TRUE)

crs <- "epsg:2272"

phl <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE) %>%
          st_transform(crs = crs)

data_path_2023 <- ("Midterm/data/2023/studentData.geojson")
data_path_2022 <- ("Midterm/data/2022/studentData.geojson")
data_2022 <- st_read(data_path_2023, quiet = TRUE)
data_2023 <- st_read(data_path_2023, quiet = TRUE)
data <- rbind(data_2022, data_2023)

drop <- c("objectid", "assessment_date", "beginning_point", "book_and_page", "category_code", 
          "cross_reference", "date_exterior_condition", "house_number", "location", "owner_1", 
          "owner_2", "parcel_number", "recording_date", "registry_number", "sale_date",
          "mailing_address_1", "mailing_address_2", "mailing_care_of", "mailing_zip", "mailing_street", 
          "mailing_city_state", "building_code", "geographic_ward", "state_code", "street_code", 
          "street_name", "street_designation", "street_direction", "musaID", "census_tract", "suffix",
          "zip_code", "building_code_new", "year_built_estimate", "pin", "toPredict", "unit", "exempt_land",
          "building_code_description"
          )

to_cat <- c("category_code_description", "garage_type")

data <- data %>%
          mutate(non_resident_owner = mailing_address_1 == mailing_street) %>%
          select(-drop) %>%
          mutate_at(to_cat, as.character) %>% 
          # mutate(house_extension = as.numeric(house_extension)) %>% # for some reason, this *really* fucks up the model
          st_transform(crs = crs) %>%
          filter(sale_price > 1) #need to filter for realistic sale prices, per https://www.phila.gov/media/20220525080608/tax-year-2023-mass-appraisal-valuation-methodology.pdf

unique(data$quality_grade)

ggplot(data, aes(x = sale_price)) +
  geom_histogram(bins = 1000)

data <- data[phl, ]
keep_columns <- sapply(data, function(col) length(unique(col)) > 1) #drop columns with only one unique value (i.e., no variance)
data <- data[, keep_columns]


### test model 

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)

keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- numeric_only[, keep_vars]

set.seed(42)
train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(sale_price ~ ., data = numeric_only,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)

print(model)

### 

hoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', quiet = T) %>%
  st_transform(crs = crs) %>%
  select(mapname)

data <- st_join(data, hoods)

acs_vars <- c("B25026_001E",
              "B02001_002E",
              "B15001_050E",
              "B15001_009E",
              "B19013_001E", 
              "B25058_001E",
              "B06012_002E")

phl_acs <- get_acs(geography = "tract",
                   variables = acs_vars, 
                   year= 2020, 
                   state= "PA",
                   county= "Philadelphia", 
                   geometry=TRUE,
                   output = "wide") %>% 
  st_transform(crs = crs) %>%
  rename(totPop = B25026_001E, 
         white = B02001_002E,
         femaleBachelors = B15001_050E, 
         maleBachelors = B15001_009E,
         medHHInc = B19013_001E, 
         medRent = B25058_001E,
         totPov = B06012_002E) %>%
  mutate(pctWhite = ifelse(totPop > 0, white / totPop, 0),
         pctBach = ifelse(totPop > 0, ((femaleBachelors + maleBachelors) / totPop), 0),
         pctPov = ifelse(totPop > 0, totPov / totPop, 0)) %>%
  dplyr::select(-white, -femaleBachelors, -maleBachelors, -totPov, -ends_with("M")) %>%
  filter(!st_is_empty(geometry)) %>%
  select(totPop, medHHInc, medRent, pctWhite, pctBach, pctPov)

saveRDS(phl_acs, "Midterm/phl_acs.RData")

data <- st_join(data, phl_acs)


### test model 

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)

keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- numeric_only[, keep_vars]

set.seed(42)
train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(sale_price ~ ., data = numeric_only,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)

print(model)

### 


# tree canopy cover
url <- "https://national-tes-data-share.s3.amazonaws.com/national_tes_share/pa.zip.zip"
tmp_file <- tempfile(fileext = ".zip")
curl_download(url, tmp_file)

unzipped_folder_1 <- tempfile()
unzip(tmp_file, exdir = unzipped_folder_1)
shp_files <- list.files(unzipped_folder_1, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
tree_canopy_gap <- st_read(shp_files[1], quiet = TRUE)  # assuming there's only one .shp file
phl_tree_canopy <- tree_canopy_gap %>%
  filter(state == "PA",
         county == "Philadelphia County") %>%
  transmute(tree_cover = 1 - tc_gap) %>%
  st_transform(crs = crs)

data <- st_join(data, phl_tree_canopy)

data <- data %>% 
          mutate(nb = st_knn(geometry, k = 15),
                 wt = st_weights(nb),
                 price_lag = st_lag(sale_price, nb, wt)) %>%
          select(-c(nb, wt))


### test model 

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)

keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- numeric_only[, keep_vars]

set.seed(42)
train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(sale_price ~ ., data = numeric_only,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)

print(model)

### 

# library(ggpubr)
# install.packages('ggpmisc')
# library(ggpmisc)
# 
# ggplot(data, aes(x = sale_price, y = price_lag3)) +
#   geom_point() +  # add points
#   geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
#   stat_poly_eq(aes(label = paste(..eq.label.., "   ", ..rr.label.., sep = "")),
#                label.x = "right", label.y = "top", formula = y ~ x, 
#                parse = TRUE)  # add equation and R^2

# nbs <- c(3, 4, 5, 6, 7, 10, 15, 20)
# cors <- c(
# cor(data$sale_price, data$price_lag3, method = "spearman"),
# cor(data$sale_price, data$price_lag4, method = "spearman"),
# cor(data$sale_price, data$price_lag5, method = "spearman"),
# cor(data$sale_price, data$price_lag6, method = "spearman"),
# cor(data$sale_price, data$price_lag7, method = "spearman"),
# cor(data$sale_price, data$price_lag10, method = "spearman"),
# cor(data$sale_price, data$price_lag15, method = "spearman"),
# cor(data$sale_price, data$price_lag20, method = "spearman")
# )

# cor_x_nb <- data.frame(nbs, cors)
# 
# ggplot(cor_x_nb, aes(x = nbs, y = cors)) +
#   geom_point() +
#   geom_smooth()

# unique_counts <- sapply(data, function(col) length(unique(col)))
# print(unique_counts)

### add proximity to commercial corridors
corridors_path <- "https://opendata.arcgis.com/datasets/f43e5f92d34e41249e7a11f269792d11_0.geojson"
corridors <- st_read(corridors_path, quiet= TRUE) %>% st_transform(crs = crs)

nearest_fts <- sf::st_nearest_feature(data, corridors)

# convert to rsgeo geometries
x <- rsgeo::as_rsgeo(origins)
y <- rsgeo::as_rsgeo(dests)

# calculate distance
data$dist_to_commerce <- rsgeo::distance_euclidean_pairwise(x, y[nearest_fts])

sample <- sample_n(data, 20)

tm_shape(corridors) +
  tm_polygons(col = 'darkgreen', border.alpha = 0, alpha = 0.5) +
tm_shape(sample) +
  tm_dots(col = 'dist_to_commerce')

origins <- corridors # st_centroid(data$geometry)
dests <- st_centroid(data$geometry) # corridors
nearest_fts <- sf::st_nearest_feature(origins, dests)
st_distance(origins, dests[nearest_fts], by_element = TRUE)

### reverse
origins <- st_centroid(data$geometry)
dests <- corridors
nearest_pts <- sf::st_nearest_points(origins, dests)
st_distance(origins, dests[nearest_fts], by_element = TRUE)


nearest_fts <- sf::st_nearest_feature(origins, dests)
origins[nearest_fts] %>%
  st_distance(dests, by_element = TRUE)

st_distance(dests[nearest_fts], origins, by_element = TRUE)


origins$nearest_fts <- sf::st_nearest_feature(origins, dests)
origins$dist_to_commerce <- st_distance(dests, origins[nearest_fts], by_element = TRUE)

origins <- st_centroid(data$geometry)
dests <- corridors
origins$nearest_fts <- sf::st_nearest_feature(origins, dests)
origins$dist_to_commerce <- st_distance(dests, origins[nearest_fts], by_element = TRUE)

data <- data %>%
          rowwise() %>%
          mutate(dist_to_commerce = as.numeric(min(st_distance(st_centroid(geometry), corridors$geometry)))) %>%
          ungroup()

# account for proximity to center city (bid rent model)
downtown <- c(39.95268, -75.16505) %>% st_as_sf() %>% st_transform(crs = crs)

####

dummied_data <- fastDummies::dummy_cols(st_drop_geometry(data))

dummied_data <- dummied_data %>%
                    mutate_if(is.factor, as.numeric) %>%
                    select_if(~ !is.character(.))

  # crime raster layers
  # flooding?
  # race
  # income (these will be colinear)
  # level of education?

# str(dummied_data)




model <- lm(sale_price ~ ., data = dummied_data)
keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- dummied_data[, keep_vars]


numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric)) %>% select(-tree_cover)
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)
ols_step_both_p(model)


  # proximity to parks
  # proximity to commercial corridor
keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- dummied_data[, keep_vars]

set.seed(42)
train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(sale_price ~ ., data = numeric_only,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)
?train

print(model)

# dummy vars: https://stackoverflow.com/questions/48649443/how-to-one-hot-encode-several-categorical-variables-in-r