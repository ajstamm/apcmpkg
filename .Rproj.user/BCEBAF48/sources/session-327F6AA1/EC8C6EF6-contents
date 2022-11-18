#' Draw a forest plot of air pollutant by green space
#'
#' @param data        Dataset of model settings, including lag RRs and CIs.
#' @param defect_abbr Abbreviation for birth defect to be plotted, from the
#'                    dataset.
#' @param lags        Maximum number of lags in the model.
#' @param defect_name Full name of the defect, for the graph title.
#'
#' @description
#' This function reads in a table with risk ratios and confidence intervals by
#' lag to be plotted on a graph, then calls `draw_aim2_forest_plot()` to
#' create two ggplot objects (one for mean concentrations and one for peaks)
#' and uses a grob function to combine them into one figure.
#'
#'
#'
#'


draw_aim2_forest <- function(data, defect_abbr, lags, defect_name = "") {
  my_lags <- lags
  d <- data |> dplyr::filter(bd == defect_abbr, measure == "mean",
                             lags == my_lags)
  x <- draw_aim2_forest_plot(data = d, title = "(a) Mean models")
  d <- data |> dplyr::filter(bd == defect_abbr, measure == "peak",
                             lags == my_lags)
  y <- draw_aim2_forest_plot(data = d, title = "(b) Peak models")

  main <- paste(defect_name, paste0(lags, "-week"), "models: \n",
                "Risk ratios by week of pregnancy,",
                "300m buffer with grasses and trees")
  if (defect_abbr == "cf") {
    footer <- "Note: For clubfoot, only the 16-week model was run. \n"
  } else {
    footer <- "Note:"
  }
  footer <- paste(footer, "Model was adjusted for maternal education level,",
                  "maternal smoking, \n tract-level median income,",
                  "conception season, and the indicated green space variable. \n")
  if (lags == 12) {
    footer <- paste(footer,
                    "Lag 0 was the end of the second month of pregnancy.",
                    "Lag 8 was week of conception.")
  } else {
    footer <- paste(footer,
                    "Lag 0 was the end of the first trimester of pregnancy.",
                    "Lag 12 was week of conception.")
  }

  return(gridExtra::grid.arrange(ggplot2::ggplotGrob(x),
                                 ggplot2::ggplotGrob(y),
                                 top = main, bottom = footer, ncol = 1))

  # return grid directly to avoid tablegrob comment
  # return(gridExtra::grid.arrange(x, y, ncol = 1))
}
