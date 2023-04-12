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


draw_forest_wrapper <- function(data, defect_abbr, defect_name = "",
                                green = "") {
  d <- dplyr::filter(data, defect == defect_abbr, !lag == "cum")
  d <- dplyr::mutate(d, lag = (as.numeric(gsub("lag", "", lag)) * -1) +
                       (lags - 4))
  min <- floor(min(d$cilow)*10)/10
  max <- ceiling(max(d$cihigh)*10)/10
  # read each plot
  dm12 <- dplyr::filter(d, grepl("mean", air), lags == 12)
  m12 <- draw_forest_plot(data = dm12, title = "(a) Mean 12-week models",
                          miny = min, maxy = max)
  dp12 <- dplyr::filter(d, grepl("peak", air), lags == 12)
  p12 <- draw_forest_plot(data = dp12, title = "(b) Peak 12-week models",
                          miny = min, maxy = max)
  dm16 <- dplyr::filter(d, grepl("mean", air), lags == 16)
  m16 <- draw_forest_plot(data = dm16, title = "(c) Mean 16-week models",
                          miny = min, maxy = max)
  dp16 <- dplyr::filter(d, grepl("peak", air), lags == 16)
  p16 <- draw_forest_plot(data = dp16, title = "(d) Peak 16-week models",
                          miny = min, maxy = max, legend = TRUE)

  main <- paste(defect_name, "models: \n", "Risk ratios by week of pregnancy")
  if (green == "gt") {
    main <- paste0(main, ", 300m buffer with grasses and trees")
    footer <- paste("Note: Models were adjusted for maternal education level,",
                    "maternal smoking, tract-level median income,",
                    "conception season, and the indicated green space variable. ")
  } else if (green == "gw") {
    main <- paste0(main, ", 300m buffer with grasses, trees, and water")
    footer <- paste("Note: Models were adjusted for maternal education level,",
                    "maternal smoking, tract-level median income,",
                    "conception season, and the indicated green space variable. ")
  } else {
    footer <- paste("Note: Models were adjusted for maternal education level,",
                    "maternal smoking, tract-level median income, and ",
                    "conception season. ")
  }
  footer <- paste(footer, "Week 0 was the week of conception. ",
                  "Week 8 was the end of the second month of pregnancy. ",
                  "Week 12 was the end of the first trimester of pregnancy. ",
                  "Risk ratio applies to a 10-unit increase from the previous",
                  "week in the air pollutant (ppb for ozone and ",
                  "µg/m\U00B3 for PM\U2082.\U2085) over two standard",
                  "deviations above the mean.")

  # to calculate the length of the string with cex = 1
  # inches_of_text <- graphics::strwidth(footer, units = "inch", family = "serif")

  # fontfamily = "Times New Roman" fails with letters stacking illegibly
  foot <- gridtext::textbox_grob(footer, width = unit(7, "in"),
                                 x = unit(0.5, "npc"), y = unit(0.7, "npc"),
                                 gp = grid::gpar(fontfamily = "serif",
                                                 justify = "left",
                                                 fontsize = 10),
                                 r = unit(5, "pt"),
                                 padding = unit(c(10, 10, 10, 10), "pt"),
                                 margin = unit(c(0, 10, 0, 10), "pt"))


  return(gridExtra::grid.arrange(ggplot2::ggplotGrob(m12),
                                 ggplot2::ggplotGrob(p12),
                                 ggplot2::ggplotGrob(m16),
                                 ggplot2::ggplotGrob(p16),
                                 foot, # widths = 6,
                                 layout_matrix = cbind(1:5),
                                 ncol = 1, # top = main, bottom =
                                 heights = c(rep(1.5, 3), 2, 0.5)))

  # return grid directly to avoid tablegrob comment
  # return(gridExtra::grid.arrange(x, y, ncol = 1))
}
