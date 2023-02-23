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
#' lag to be plotted on a graph, then calls `draw_forest_plot()` to
#' create two ggplot objects (one for mean concentrations and one for peaks)
#' and uses a grob function to combine them into one figure.
#'
#'
#'
#'


draw_forest_wrapper <- function(data, defect_abbr, lags, defect_name = "",
                                green = "") {
  my_lags <- lags
  d <- dplyr::filter(data, defect == defect_abbr, lags == my_lags,
                     !lag == "cum")
  d <- dplyr::mutate(d, lag = (as.numeric(gsub("lag", "", lag)) * -1) +
                       (lags - 4))
  dx <- dplyr::filter(d, grepl("mean", air))
  x <- draw_forest_plot(data = dx, title = "(a) Mean models")
  dy <- dplyr::filter(d, grepl("peak", air))
  y <- draw_forest_plot(data = dy, title = "(b) Peak models")

  main <- paste(defect_name, paste0(lags, "-week"), "models: \n",
                "Risk ratios by week of pregnancy")
  if (green == "gt") {
    main <- paste0(main, ", 300m buffer with grasses and trees")
    footer <- paste("Note:", "Model was adjusted for maternal education level,",
                    "maternal smoking, \n tract-level median income,",
                    "conception season, and the indicated green space variable. \n")
  } else if (green == "gw") {
    main <- paste0(main, ", 300m buffer with grasses, trees, and water")
    footer <- paste("Note:", "Model was adjusted for maternal education level,",
                    "maternal smoking, \n tract-level median income,",
                    "conception season, and the indicated green space variable. \n")
  } else {
    footer <- paste("Note:", "Model was adjusted for maternal education level,",
                    "maternal smoking, \n tract-level median income, and ",
                    "conception season. \n")
  }
  if (lags == 12) {
    footer <- paste(footer,
                    "Week 0 was the week of conception.",
                    "Week 8 was the end of month 2 of pregnancy.")
  } else {
    footer <- paste(footer,
                    "Week 0 was the week of conception.",
                    "Week 12 was the end of the first trimester of pregnancy.")
  }

  return(gridExtra::grid.arrange(ggplot2::ggplotGrob(x),
                                 ggplot2::ggplotGrob(y),
                                 top = main, bottom = footer, ncol = 1))

  # return grid directly to avoid tablegrob comment
  # return(gridExtra::grid.arrange(x, y, ncol = 1))
}
