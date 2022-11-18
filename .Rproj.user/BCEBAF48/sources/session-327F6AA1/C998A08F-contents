#' Draw individual plots to include in the double-plot figure
#'
#' @param data  Dataset to plot, with RRs and CIs.
#' @param title Title for the individual plot.
#'
#' @description
#' This function draws the risk ratios and confidence intervals by lag
#' for each air pollutant on the same graph. It returns a ggplot object.
#'

draw_aim2_forest_plot <- function(data, title) {
  my_x <- c(0, round(max(data$lag)))

  #mean profiles in the same graphs
  mp <- ggplot2::ggplot(data, aes(x=lag, y=rr, group = air, fill = air, col = air)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
          text = ggplot2::element_text(size = 12),
          plot.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::geom_vline(xintercept = seq(from = 0, to = my_x[2], by = 4)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(x = "Lag week", y = "Risk Ratio", title = title) + #
    ggplot2::guides(shape = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(from = 0, to = my_x[2], by = 2)) +
    ggplot2::scale_color_manual(labels = c("Ozone", "PM2.5"),
                       values = c("blue", "red"),
                       name = "Air pollutant") +
    ggplot2::scale_fill_manual(labels = c("Ozone", "PM2.5"),
                      values = c("blue", "red"),
                      name = "Air pollutant") +
    ggplot2::geom_errorbar(aes(ymin = cilow, ymax = cihigh), width = 0.1)
  return(mp)
}
