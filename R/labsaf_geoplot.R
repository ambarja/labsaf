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
#' @importFrom crayon %+%
#' @return  A graphic of ggplot2 o leaflet.
#' @export

labsaf_geoplot <- function(region, data, var, mode = "plot",...){

  if("EPSG:4326" == sf::st_crs(region)[[1]] &  "EPSG:4326" == sf::st_crs(data)[[1]]){
    if(mode == "plot"){
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
      } else if (mode == "view"){
        pal <- leaflet::colorNumeric(palette = "viridis",domain = data[[var]])

        inia_popup <- paste0(
          "<h3 style='margin-bottom: 0px; color:black;'><b>Soil properties</b></h3>",
          "<hr>",
          glue::glue("<b style='color:green;'>{var} value</b>: "),
          "<b style='color:blue;'>",
          data[[var]],
          "</b>"
        )

        geomap <- leaflet::leaflet(data = region) %>%
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
            fill = NULL,
            group = "region",
            popup = NULL) %>%
          leaflet::addCircleMarkers(
            data = data,
            weight = 0,
            radius = 6,
            stroke = 5,
            fillColor = ~pal(data[[var]]),
            fillOpacity = 0.8,
            popup = inia_popup,
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
        } else {
          cat(crayon::green(
            'Error in mode, ' %+%
              crayon::blue$underline$bold('labsaf_geoplot') %+%
              ' only accept' %+% crayon::bold(' plot and view mode\n')))
          }

    } else {
      cat(crayon::green(
      'Error in CRS of region or data, ' %+%
        crayon::blue$underline$bold('labsaf_geoplot') %+%
        ' only accept' %+% crayon::bold(' Geographical projection!\n')))
    }
  return(geomap)
  }


