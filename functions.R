library(sf)
library(spatstat)
library(terra)

st_kde <- function(data){
  sp_points <- as(data, "ppp")
  ppp_object <- as.ppp(sp_points)
  kde <- densityAdaptiveKernel(ppp_object)
  kde_spatraster <- rast(kde)
  return(kde_spatraster)
}

# Make a grid to sample from
grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))

# sample 100 points
pnts <- sf::st_sample(grd, 100)

kde_layer <- st_kde(pnts)

terra::plot(kde_layer)