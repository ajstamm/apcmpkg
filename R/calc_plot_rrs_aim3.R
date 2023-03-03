#' Calculate and plot everything together in defect-lags sets
#'

calc_plot_rrs_aim3 <- function(defect, lags, model, defect_name) {
  nam <- names(model)
  nam <- nam[grepl(defect, nam) & grepl(lags, nam) &
               grepl("o3", nam) & !grepl("data", nam)]
  d <- model[[paste0(defect, "_data")]]

  # calc rrs ----
  rr <- tibble::tibble(defect = as.character(), air = as.character(),
                        lags = as.numeric(), lag = as.character(),
                        rr = as.numeric(), cilow = as.numeric(),
                        cihigh = as.numeric(), threshold = as.numeric())

  for (i in 1:2) {
    msr <- gsub("^[cfslp]+_|_lin__.+", "", nam[i])
    r <- calc_rr(data = d, defect = defect, lags = lags,
                 model = model[[nam[i]]]$mgtcov,
                 cb1 = model[[nam[i]]]$mgtcb1,
                 measure = msr)
    rr <- dplyr::bind_rows(rr, r)
    r <- calc_rr(data = d, defect = defect, lags = lags,
                 model = model[[nam[i]]]$mgtcov,
                 cb2 = model[[nam[i]]]$mgtcb2,
                 measure = gsub("o3", "pm", msr))
    rr <- dplyr::bind_rows(rr, r)
  }

  # no cumulative RRs are significant
  write.csv(rr, paste("data/model3_rr", defect, lags, Sys.Date(), ".csv",
                      sep = "_"))


  # draw plots ----
  p0 <- list()
  p <- draw_forest_wrapper(data = rr, defect_abbr = defect, lags = lags,
                           defect_name = defect_name, green = "")
  p0$forest = grDevices::recordPlot(ggpubr::as_ggplot(p))

  p1 <- cross_plot(list = model, bd = defect, air = "o3", measure = "mean",
                   lags = lags, centre = 58.67)
  p2 <- cross_plot(list = model, bd = defect, air = "o3", measure = "peak",
                   lags = lags, centre = 78.98)
  p3 <- cross_plot(list = model, bd = defect, air = "pm", measure = "mean",
                   lags = lags, centre = 17.80)
  p4 <- cross_plot(list = model, bd = defect, air = "pm", measure = "peak",
                   lags = lags, centre = 31.47)

  plots <- c(p0, p1, p2, p3, p4)

  # save plots ----
  plotfile <- paste("images/crossplots_aim3", defect, lags, Sys.Date(),
                    ".pdf", sep = "_")
  grDevices::pdf(plotfile, onefile=TRUE, width = 10, height = 7)
  for (myplot in plots) grDevices::replayPlot(myplot)
  grDevices::dev.off() # need to close pdf file

}
