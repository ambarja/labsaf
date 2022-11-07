#' labsaf geoplot
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This is a function to create a point map with the values of the soil properties.
#' @param region, is an sf object of type polygon that represent the study area.
#' @param data, is an sf object with the values of the **soil properties**.
#' @param var, represent the values of the soil properties.
#' @param ..., further arguments of `labs` or `anotation_*` of the ggplot2 and ggspatial package.
#' @return  A graphic of ggplot2.
#' @export

labsaf_geoplot <- function(
    region, data, var,...){
  geomap <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = region,
      fill = "white",
      color = "black"
    ) +
    ggplot2::geom_sf(
      data = data,
      size = 3,
      shape = 16,
      ggplot2::aes(color = data[[var]])
    ) +
    ggplot2::labs(color = var,...) +
    ggspatial::annotation_north_arrow(location="tl",...) +
    ggspatial::annotation_scale(location="br",...)
  return(geomap)
}
