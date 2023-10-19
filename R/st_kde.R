#' Create a Kernel Density Estimation (KDE) spatraster layer
#'
#' This function calculates the Kernel Density Estimation (KDE) using an adaptive kernel
#' based on a set of points in an sf object. The resulting KDE is returned as a spatraster
#' layer.
#'
#' @param data An sf object containing points.
#'
#' @return A spatraster layer of the KDE.
#'
#' @examples
#'
#' # make a grid to sample from
#' grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0)) %>% st_as_sf()
#' 
#' # sample 100 points
#' pnts <- sf::st_sample(grd, 100) %>% st_as_sf()
#' 
#' # plot
#' terra::plot(kde_layer)
#'
#' @import sf
#' @import spatstat.geom
#' @import spatstat.explore
#' @import terra
#'
#' @export

st_kde <- function(x){
  if (!inherits(x, "sf")) {
    cli::cli_abort("{.arg x} must be of class {.cls sf}")
  }
  
  ppp_object <- spatstat.geom::as.ppp(x)
  kde <- spatstat.explore::densityAdaptiveKernel(ppp_object)
  terra::rast(kde)
}

