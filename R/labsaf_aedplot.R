#' labsaf aed
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This is a function to create a some plot of analysis exploratory of data.
#' @param data, is an sf or data.frame object with the values of the **soil properties**.
#' @param var, represent the values of the soil properties.
#' @param fill, is a fill color hexadecimal for plot.
#' @import patchwork
#' @return  A graphic of ggplot2.
#' @export

labsaf_eadplot <- function(data, var, fill){
  plot_hist <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = data[[var]])) +
    ggplot2::geom_histogram(fill = "#69782e",bins = 9,alpha=0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = sprintf("Histogram of %s",var),x="",y="")

  plot_boxplot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x=var,y = data[[var]])
  ) +
    ggplot2::geom_violin(fill = "#69782e", alpha=0.7) +
    ggplot2::geom_jitter() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = sprintf("Violin plot of %s",var),x="",y="")
  panel_plot <- plot_hist | plot_boxplot
  return(panel_plot)
}
