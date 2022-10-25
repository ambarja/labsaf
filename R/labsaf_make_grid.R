#' labsaf make grid
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This is a function to create a grid regular o hexagonal of the study area.
#' @param x, is as sf object of type polygon (*UTM project*) that represent the study area.
#' @param size, is the size of the pixel grid in **meters**.
#' @param square, is logical; if `FALSE`, create **hexagonal grid**.
#' @param ..., further arguments of `st_make_grid()` of the sf package.
#' @return  An sf object of type polygons.
#' @export

labsaf_make_grid <- function(x, size, square = TRUE, ...){
  grid <- sf::st_make_grid(
    x,
    size,
    what = "polygons",
    square = TRUE,
    ...
    )
  id_grid <- data.frame(ID = 1:length(grid))
  sf::st_geometry(id_grid) <- grid
  output <- id_grid[x,]
  return(output)
}
