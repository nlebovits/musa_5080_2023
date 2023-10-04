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
library(rsgeo)
library(janitor)
library(spatstat)
library(maptools)
library(terra)

tmap_mode('view')
options(tigris_use_cache = TRUE)

crs <- "epsg:2272"

phl <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE) %>%
          st_transform(crs = crs)

data_path <- ("Midterm/data/2023/studentData.geojson")
data <- st_read(data_path, quiet = TRUE)


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
          filter(sale_price > 1) %>% #need to filter for realistic sale prices, per https://www.phila.gov/media/20220525080608/tax-year-2023-mass-appraisal-valuation-methodology.pdf
          mutate(number_of_rooms = ifelse(is.na(number_of_rooms), 1, number_of_rooms))

# colnames(data)
# 
# ggplot(data) +
#   geom_bar(aes(y = building_code_description_new))
# 
# price_x_type <- data %>%
#                   group_by(building_code_description_new) %>%
#                   summarize(avg_price = mean(sale_price),
#                             median_price = median(sale_price),
#                             count = n())
# 
# ggplot(price_x_type, aes(x = avg_price, y = median_price)) +
#   geom_point()
# 
# ggplot(price_x_type) +
#   geom_col(aes(y = reorder(building_code_description_new, median_price), x = median_price))
# 
# ggplot(data, aes(x = sale_price)) +
#   geom_histogram(bins = 1000)

data <- data[phl, ]
keep_columns <- sapply(data, function(col) length(unique(col)) > 1) #drop columns with only one unique value (i.e., no variance)
data <- data[, keep_columns]


### test model 

# numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
# model <- lm(sale_price ~ ., data = numeric_only)
# # ols_step_all_possible(model)
# ols_step_both_aic(model)
# 
# keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
# final_data <- numeric_only[, keep_vars]
# 
# set.seed(42)
# train_control <- trainControl(method = "cv",
#                               number = 10)
# 
# model <- train(sale_price ~ ., data = numeric_only,
#                trControl = train_control,
#                method = "lm",
#                na.action = na.exclude)
# 
# print(model)

### 

hoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', quiet = T) %>%
  st_transform(crs = crs) %>%
  select(mapname)

data <- st_join(data, hoods)

# acs_vars <- c("B25026_001E",
#               "B02001_002E",
#               "B15001_050E",
#               "B15001_009E",
#               "B19013_001E", 
#               "B25058_001E",
#               "B06012_002E")
# 
# phl_acs <- get_acs(geography = "tract",
#                    variables = acs_vars, 
#                    year= 2020, 
#                    state= "PA",
#                    county= "Philadelphia", 
#                    geometry=TRUE,
#                    output = "wide") %>% 
#   st_transform(crs = crs) %>%
#   rename(totPop = B25026_001E, 
#          white = B02001_002E,
#          femaleBachelors = B15001_050E, 
#          maleBachelors = B15001_009E,
#          medHHInc = B19013_001E, 
#          medRent = B25058_001E,
#          totPov = B06012_002E) %>%
#   mutate(pctWhite = ifelse(totPop > 0, white / totPop, 0),
#          pctBach = ifelse(totPop > 0, ((femaleBachelors + maleBachelors) / totPop), 0),
#          pctPov = ifelse(totPop > 0, totPov / totPop, 0)) %>%
#   dplyr::select(-white, -femaleBachelors, -maleBachelors, -totPov, -ends_with("M")) %>%
#   filter(!st_is_empty(geometry)) %>%
#   select(totPop, medHHInc, medRent, pctWhite, pctBach, pctPov)
# 
# saveRDS(phl_acs, "Midterm/phl_acs.RData")

phl_acs <- readRDS("Midterm/phl_acs.RData")

data <- st_join(data, phl_acs)


### test model 

# numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
# model <- lm(sale_price ~ ., data = numeric_only)
# # ols_step_all_possible(model)
# ols_step_both_aic(model)
# 
# keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
# final_data <- numeric_only[, keep_vars]
# 
# set.seed(42)
# train_control <- trainControl(method = "cv",
#                               number = 10)
# 
# model <- train(sale_price ~ ., data = numeric_only,
#                trControl = train_control,
#                method = "lm",
#                na.action = na.exclude)
# 
# print(model)

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

# numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
# model <- lm(sale_price ~ ., data = numeric_only)
# # ols_step_all_possible(model)
# ols_step_both_aic(model)
# 
# keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
# final_data <- numeric_only[, keep_vars]
# 
# set.seed(42)
# train_control <- trainControl(method = "cv",
#                               number = 10)
# 
# model <- train(sale_price ~ ., data = numeric_only,
#                trControl = train_control,
#                method = "lm",
#                na.action = na.exclude)
# 
# print(model)

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
x <- rsgeo::as_rsgeo(data)
y <- rsgeo::as_rsgeo(corridors)

# calculate distance
data$dist_to_commerce <- rsgeo::distance_euclidean_pairwise(x, y[nearest_fts])

# sample <- sample_n(data, 20)
# 
# tm_shape(corridors) +
#   tm_polygons(col = 'darkgreen', border.alpha = 0, alpha = 0.5) +
# tm_shape(sample) +
#   tm_dots(col = 'dist_to_commerce') +
# tm_shape(point_sf) +
#   tm_dots()


downtown <- st_sfc(st_point(c(-75.16408, 39.95266)), crs = 4326)
downtown_sf <- st_sf(geometry = downtown)
downtown_sf <- downtown_sf %>% st_transform(crs= crs)

nearest_fts <- sf::st_nearest_feature(data, downtown_sf)

# convert to rsgeo geometries
x <- rsgeo::as_rsgeo(data)
y <- rsgeo::as_rsgeo(downtown_sf)

# calculate distance
data$dist_to_downtown <- rsgeo::distance_euclidean_pairwise(x, y[nearest_fts])

#### shootings


shootings_url <- 'https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings+WHERE+year+%3E+2018&filename=shootings&format=geojson&skipfields=cartodb_id'
shootings <- st_read(shootings_url) %>% 
                  st_transform(crs = crs) %>%
                  filter(!st_is_empty(geometry))

sp_points <- as(shootings, "Spatial")
ppp_object <- as.ppp(sp_points)
kde <- densityAdaptiveKernel(ppp_object)
kde_spatraster <- rast(kde)

guncrime <- terra::extract(kde_spatraster, data) %>%
              transmute(ID = ID,
                        guncrime_density = scale(lyr.1))
data <- cbind(data, guncrime) 


# sample <- sample_n(data, 100)
# 
# tm_shape(sample) +
#   tm_dots(col = "guncrime_density")
# 
# colnames(data)



  # crime raster layers
  # flooding?
  # race
  # income (these will be colinear)
  # level of education?

# str(dummied_data)





set.seed(42)

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric)) 
model <- lm(sale_price ~ ., data = numeric_only)
keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price", "mapname", "building_code_description_new", "quality_grade")

final_data <- data %>% select(all_of(keep_vars)) %>% st_drop_geometry()

dummied_data <- dummy_cols(final_data) %>%
                    clean_names() %>%
                    select(-c(mapname,
                              building_code_description_new,
                              quality_grade))

model <- lm(sale_price ~ ., data = dummied_data)
keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")

final_data <- dummied_data %>% select(keep_vars) %>% st_drop_geometry()



customSummary <- function(data, lev = NULL, model = NULL) {
  mpe <- mean((data$obs - data$pred) / data$obs) * 100
  mae <- mean(abs(data$obs - data$pred))
  rmse <- sqrt(mean((data$obs - data$pred)^2))
  rsq <- cor(data$obs, data$pred)^2
  out <- c(MAE = mae, RMSE = rmse, Rsquared = rsq, MPE = mpe)
  out
}

train_control <- trainControl(method = "cv",
                              number = 100,
                              summaryFunction = customSummary)


model <- train(sale_price ~ ., 
               data = final_data,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)

# predict(model)

print(model)

final_data %>%
  group_by(number_of_rooms) %>%
  summarize(n())

reg = lm(sale_price ~ ., data = final_data)
summary(reg)
length(rstandard(reg))

data_nb = data_nb |>
  mutate(stand_resids = rstandard(reg),
         spatial_lag = st_lag(stand_resids, 
                              nb, 
                              wt))

### calculate global moran's i of sale price
data_nb <- data %>% 
          mutate(nb = st_knn(geometry, k = 5),
                 wt = st_weights(nb),
                 .before = 1)
# library(spdep)
# 
# nb_lines = nb2lines(data_nb$nb, 
#                           coords = st_centroid(data_nb$geometry), 
#                           as_sf = TRUE)
# 
# tmap_mode('plot')
# 
# tm_shape(phl) + 
#   tm_borders(col = "grey", lwd = 0.5) + 
#   tm_shape(nb_lines) +
#   tm_lines(col = "red") +
#   tm_layout(frame = FALSE)

# calculate global moran's i for sale_price
global_moran(data_nb$sale_price, data_nb$nb, data_nb$wt)$`I`

moranMC = global_moran_perm(data_nb$sale_price, data_nb$nb, data_nb$wt, alternative = "two.sided", 999)

moranMC

moranMCres = moranMC$res |>
  as.data.frame()

colnames(moranMCres) = "col1"

ggplot(moranMCres) +
  geom_histogram(aes(col1), bins = 100) +
  geom_vline(xintercept = moranMC$statistic, col = "red") +
  labs(title = "Histogram of MoranMCres",
       x = "moranMCres",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

moran.plot(data_nb$sale_price, nb2listw(data_nb$nb),
           xlab = "Sale Price", 
           ylab = "Spatial Lag")

lisa = local_moran(data_nb$sale_price, data_nb$nb, data_nb$wt, nsim = 999)
data_nb <- cbind(data_nb, lisa)

lisa_sample <- sample_n(data_nb, 100)

tmap_mode('view')

tm_shape(lisa_sample) +
  tm_dots(col = "p_ii")