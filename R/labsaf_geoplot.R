#' labsaf geoplot
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This is a function to create a point map with the values of the soil properties.
#' @param region, is an sf object of type polygon that represent the study area.
#' @param data, is an sf object with the values of the **soil properties**.
#' @param var, represent the values of the soil properties.
#' @param mode, is the output mode static o view plot.
#' @param ..., further arguments of `labs` or `anotation_*` of the ggplot2 and ggspatial package.
#' @importFrom rlang .data
#' @return  A graphic of ggplot2.
#' @export

labsaf_geoplot <- function(region, data, var, mode,...){
  if(mode=="plot"){
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

  }else{
    pal <- leaflet::colorNumeric(palette = "viridis",domain = data[[var]])
    geoplot <- leaflet::leaflet(data = region) %>%
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap,
        group = "OpenStreetMap") %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        group = "CartoDB.Positron") %>%
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.DarkMatter,
        group = "CartoDB.DarkMatter") %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = "ESRI.Satelital") %>%
      leaflet::addPolygons(
        color = "black",
        weight = 1,
        opacity = 1,
        fill = NA,
        group = "region") %>%
      leaflet::addCircleMarkers(
        data = data,
        weight = 0,
        radius = 5,stroke = 5,
        fillColor = ~pal(data[[var]]),
        fillOpacity = 0.8,
        label = data$PH,
        group = var
      ) %>%
      leaflet::addLegend(
        data = data,
        pal = pal,
        opacity = 1,
        values = data[[var]],
        title = var) %>%
      leaflet::addLayersControl(
        overlayGroups = c(var,"region"),
        baseGroups = c(
          "OpenStreetMap",
          "CartoDB.Positron",
          "CartoDB.DarkMatter",
          "ESRI.Satelital"),
        position = "topleft"
      )
  }
  return(geomap)
}
