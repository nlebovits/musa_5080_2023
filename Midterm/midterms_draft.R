library(tidyverse)
library(olsrr)
library(sf)
library(caret) # add dummy vars


crs <- "epsg:2272"

phl <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson", quiet = TRUE) %>%
          st_transform(crs = crs)

data_path <- ("C:/Users/nissi/Desktop/musa_5080_2023/Midterm/data/2023/studentData.geojson")
data <- st_read(data_path, quiet = TRUE)

drop <- c("objectid", "assessment_date", "beginning_point", "book_and_page", "category_code", 
          "cross_reference", "date_exterior_condition", "house_number", "location", "owner_1", 
          "owner_2", "parcel_number", "recording_date", "registry_number", "sale_date",
          "mailing_address_1", "mailing_address_2", "mailing_care_of", "mailing_zip", "mailing_street", 
          "mailing_city_state", "building_code", "geographic_ward", "state_code")

to_cat <- c("category_code_description", "census_tract", "exempt_land", "fireplaces", "garage_type")

data <- data %>%
          mutate(non_resident_owner = mailing_address_1 == mailing_street) %>%
          select(-drop) %>%
          mutate_at(to_cat, as.character) %>%
          st_transform(crs = crs)

data <- data[phl, ]

library(tmap)
tmap_mode('view')

sample_size <- nrow(data) * 0.01
sample_data <- data[sample(nrow(data), sample_size), ]

tm_shape(sample_data) +
  tm_symbols(col = "brown", border.alpha = 0, alpha = 0.5)

colnames(data)

unique(data$state_code)

install.packages("fastDummies")
library(fastDummies)

fastDummies::dummy_cols(data)

levels_more_than <- function(vec, num = 2) {
  n_distinct(levels(vec)) > num
}

recipe(~ ., data = st_drop_geometry(data)) %>%
  step_dummy(is.character()) %>%
  prep() %>%
  bake(new_data = NULL)

# to predict: sale_price

numeric_only <- data %>% st_drop_geometry() %>% select(where(is.numeric))

model <- lm(sale_price ~ ., data = numeric_only)

ols_step_all_possible(model)

ols_step_both_p(model)


# things to add:
  # neighborhood (azavea)
  # crime raster layers
  # flooding?
  # race
  # income (these will be colinear)
  # tree canopy cover
  # proximity to parks
  # proximity to commercial corridor



# dummy vars: https://stackoverflow.com/questions/48649443/how-to-one-hot-encode-several-categorical-variables-in-r