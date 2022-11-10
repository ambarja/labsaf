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

labsaf_eadplot <- function(data, var, fill = "#292da3", mode = "plot"){
  # Histogram plot
  plot_hist <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(fill = "#292da3", bins = 9, alpha=0.9, color = "white",lwd =0.1) +
    ggplot2::theme_minimal()
  # Violin plot
  plot_boxplot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = var,y = .data[[var]])) +
    ggplot2::geom_violin(trim = F, color = "#292da3", fill = "#292da3", alpha=0.9) +
    ggplot2::geom_boxplot(
      width=.1,
      fill = "white",
      color = "black") +
    ggplot2::geom_jitter(
      shape = 16,
      position = ggplot2::position_jitter(0.02),
      color = "white",
      alpha=0.5,
      size = 2) +
    ggplot2::theme_minimal()

  # Stactic plot
  if(mode == "plot"){
    plot_hist <- plot_hist +
      ggplot2::labs(
        title = sprintf("Histogram of %s",var),
        x="",
        y=""
        )

    plot_boxplot <- plot_boxplot +
      ggplot2::labs(
        title = sprintf("Violin plot of %s",var),
        x="",
        y=""
        )

    panel_plot <- plot_hist | plot_boxplot
  }
  # Interactive plot
  if(mode == "view") {
    plot_hist <- plot_hist %>% plotly::ggplotly()
    plot_boxplot <- plot_boxplot %>% plotly::ggplotly()
    panel_plot <- plotly::subplot(plot_hist, plot_boxplot) %>%
      plotly::layout(
        title = "",
        annotations = list(
          list(
            text = sprintf("Histogram of %s",var),
            x = 0,
            y = 1.05,
            showarrow = F,
            xref='paper',
            yref='paper',
            align = "right"
            ),
          list(
            text = sprintf("Violin plot of %s",var),
            x = 1,
            y = 1.05,
            showarrow = F,
            xref='paper',
            yref='paper'
            )
          )
        ) %>%  plotly::config(displayModeBar = F)
  }
  return(panel_plot)
}
