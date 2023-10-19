library(janitor)
library(sf)
library(arcpullr)

# Define the function to assign datasets to objects in the environment
assign_dataset <- function(url, name) {
  tryCatch({
    dataset <- get_spatial_layer(url)
    assign(name, dataset, envir = .GlobalEnv)
  }, error = function(err) {
    if (grepl("return_geometry is NULL", conditionMessage(err))) {
      dataset <- get_table_layer(url)
      assign(name, dataset, envir = .GlobalEnv)
    } else {
      stop(err)
    }
  })
}

get_arc <- function(server_url, dataset_list){
  queries <- '/FeatureServer/0'
  names <- dataset_list %>% make_clean_names()
  
  url_list <- list()
  
  for (dataset in dataset_list) {
    url <- paste0(server_url, dataset, queries)
    url_list <- c(url_list, url)
  }
  
  Map(assign_dataset, url_list, names)
}
