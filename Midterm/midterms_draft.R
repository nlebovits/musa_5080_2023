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

data_path <- ("Midterm/data/2023/studentData.geojson")

drop <- c("objectid", "assessment_date", "beginning_point", "book_and_page", "category_code", 
          "cross_reference", "date_exterior_condition", "house_number", "location", "owner_1", 
          "owner_2", "parcel_number", "recording_date", "registry_number", "sale_date",
          "mailing_address_1", "mailing_address_2", "mailing_care_of", "mailing_zip", "mailing_street", 
          "mailing_city_state", "building_code", "geographic_ward", "state_code", "street_code", 
          "street_name", "street_designation", "street_direction", "musaID", "census_tract", "suffix",
          "zip_code", "building_code_new", "year_built_estimate", "pin", "toPredict", "unit")

to_cat <- c("category_code_description", "exempt_land", "fireplaces", "garage_type")

data <- st_read(data_path, quiet = TRUE)%>%
          mutate(non_resident_owner = mailing_address_1 == mailing_street) %>%
          select(-drop) %>%
          mutate_at(to_cat, as.character) %>%
          st_transform(crs = crs)

data <- data[phl, ]
keep_columns <- sapply(data, function(col) length(unique(col)) > 1) #drop columns with only one unique value (i.e., no variance)
data <- data[, keep_columns]

# sample_size <- nrow(data) * 0.01
# sample_data <- data[sample(nrow(data), sample_size), ]
# 
# tm_shape(sample_data) +
#   tm_symbols(col = "brown", border.alpha = 0, alpha = 0.5)
# 
# colnames(data)

data <- data %>% 
          mutate(nb3 = st_knn(geometry, k = 3),
                 wt3 = st_weights(nb3),
                 price_lag3 = st_lag(sale_price, nb3, wt3),
                 nb4 = st_knn(geometry, k = 4),
                 wt4 = st_weights(nb4),
                 price_lag4 = st_lag(sale_price, nb4, wt4),
                 nb5 = st_knn(geometry, k = 5),
                 wt5 = st_weights(nb5),
                 price_lag5 = st_lag(sale_price, nb5, wt5),
                 nb6 = st_knn(geometry, k = 6),
                 wt6 = st_weights(nb6),
                 price_lag6 = st_lag(sale_price, nb6, wt6),
                 nb7 = st_knn(geometry, k = 7),
                 wt7 = st_weights(nb7),
                 price_lag7 = st_lag(sale_price, nb7, wt7),
                 nb10 = st_knn(geometry, k = 10),
                 wt10 = st_weights(nb10),
                 price_lag10 = st_lag(sale_price, nb10, wt10),
                 nb15 = st_knn(geometry, k = 15),
                 wt15 = st_weights(nb15),
                 price_lag15 = st_lag(sale_price, nb15, wt15),
                 nb20 = st_knn(geometry, k = 20),
                 wt20 = st_weights(nb20),
                 price_lag20 = st_lag(sale_price, nb20, wt20))

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

nbs <- c(3, 4, 5, 6, 7, 10, 15, 20)
cors <- c(
cor(data$sale_price, data$price_lag3, method = "spearman"),
cor(data$sale_price, data$price_lag4, method = "spearman"),
cor(data$sale_price, data$price_lag5, method = "spearman"),
cor(data$sale_price, data$price_lag6, method = "spearman"),
cor(data$sale_price, data$price_lag7, method = "spearman"),
cor(data$sale_price, data$price_lag10, method = "spearman"),
cor(data$sale_price, data$price_lag15, method = "spearman"),
cor(data$sale_price, data$price_lag20, method = "spearman")
)

cor_x_nb <- data.frame(nbs, cors)

ggplot(cor_x_nb, aes(x = nbs, y = cors)) +
  geom_point() +
  geom_smooth()

unique_counts <- sapply(data, function(col) length(unique(col)))
print(unique_counts)

dummmied_data <- fastDummies::dummy_cols(st_drop_geometry(data))

data <- data %>%
  select(-c(nb3, wt3, price_lag3, nb4, wt4, price_lag4, nb5, wt5, price_lag5, 
            nb6, wt6, price_lag6, nb7, wt7, price_lag7, nb10, wt10, price_lag10,
            nb15, wt15, nb20, wt20, price_lag20))

# to predict: sale_price

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)


# things to add:
  # neighborhood (azavea)
hoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', quiet = T) %>%
          st_transform(crs = crs) %>%
          select(mapname)

data <- st_join(data, hoods)



  # crime raster layers
  # flooding?
  # race
  # income (these will be colinear)
  # level of education?





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

data <- st_join(data, phl_acs)

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
# ols_step_all_possible(model)
ols_step_both_aic(model)

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

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))
model <- lm(sale_price ~ ., data = numeric_only)
keep_vars <- c(ols_step_both_aic(model)$predictors, "sale_price")
final_data <- data[, keep_vars] %>% st_drop_geometry() %>%
                mutate(exterior_condition = as.factor(exterior_condition),
                       interior_condition =  as.factor(interior_condition))

predict_model <- lm(sale_price ~ ., data = final_data)
predicted_price <- predict(predict_model, newdata = variable_price)

  # proximity to parks
  # proximity to commercial corridor

set.seed(42)
train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(sale_price ~ ., data = final_data,
               trControl = train_control,
               method = "lm",
               na.action = na.exclude)
print(model)

# dummy vars: https://stackoverflow.com/questions/48649443/how-to-one-hot-encode-several-categorical-variables-in-r