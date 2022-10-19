#' labsaf make grid
#' @description
#' Is a function to create a grid of study area.
#' @param x, is a sf object (polygon) that represent the study area.
#' @param size, is the size of the pixel grid.
#' @return  a sf object (polygons).
#' @importFrom  sf st_make_grid st_geometry
#' @export
labsaf_make_grid <- function(x, size){
  grid <- st_make_grid(x, size, what = "polygons", square = TRUE)
  id_grid <- data.frame(ID = 1:length(grid))
  st_geometry(id_grid) <- grid
  output <- id_grid[x,]
  return(output)
}
