#' labsaf aed
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This is a function to create a some plot of analysis exploratory of data.
#' @param data, is an sf or data.frame object with the values of the **soil properties**.
#' @param var, represent the values of the soil properties.
#' @param mode, is the output mode static o view plot.
#' @param fill, is a fill color hexadecimal for plot.
#' @import patchwork
#' @importFrom rlang .data
#' @return  A graphic of ggplot2 or plotly.
#' @export

labsaf_eadplot <- function(data, var, fill, mode = "plot"){
  plot_hist <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(fill = "#69782e",bins = 9,alpha=0.7) +
    ggplot2::theme_minimal()

  plot_boxplot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x=var,y = .data[[var]])) +
    ggplot2::geom_boxplot(fill = "#69782e", alpha=0.7) +
    ggplot2::geom_jitter() +
    ggplot2::theme_minimal()

  if(mode == "plot"){
    plot_hist <- plot_hist +
      ggplot2::labs(title = sprintf("Histogram of %s",var),x="",y="")
    plot_boxplot <- plot_boxplot +
      ggplot2::labs(title = sprintf("Violin plot of %s",var),x="",y="")
    panel_plot <- plot_hist | plot_boxplot
  } else{
    plot_hist <- plot_hist %>% plotly::ggplotly()
    plot_boxplot <- plot_boxplot %>% plotly::ggplotly()
    panel_plot <- plotly::subplot(plot_hist,plot_boxplot) %>%
      plotly::layout(
        annotations = list(
          list(x = 0.2 , y = 1.05, text = sprintf("Histogram of %s",var), showarrow = F, xref='paper', yref='paper'),
          list(x = 0.8 , y = 1.05, text = sprintf("Violin plot of %s",var), showarrow = F, xref='paper', yref='paper'))
        )
  }
  return(panel_plot)
}
