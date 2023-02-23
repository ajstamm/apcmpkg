#' Calculate and plot everything together in defect-lags sets
#' 

calc_plot_rrs <- function(defect, lags, model, defect_name) {
  nam <- names(model)
  nam <- nam[grepl(defect, nam) & grepl(lags, nam) & !grepl("data", nam)]
  d <- model[[paste0(defect, "_data")]]
  
  # calc rrs ----
  rr1 <- tibble::tibble(defect = as.character(), air = as.character(), 
                        lags = as.numeric(), lag = as.character(), 
                        rr = as.numeric(), cilow = as.numeric(), 
                        cihigh = as.numeric(), threshold = as.numeric())
  
  # aim 1
  for (i in 1:length(nam)) {
    r <- calc_rr(data = d, defect = defect, lags = lags, 
                 model = model[[nam[i]]]$mcov, 
                 cb = model[[nam[i]]]$mccb1, 
                 measure = gsub("^[cfslp]+_|_lin__.+", "", nam[i]))
    rr1 <- dplyr::bind_rows(rr1, r)
  }
  
  # no cumulative RRs are significant
  write.csv(rr1, paste("data/model1_rr", defect, lags, Sys.Date(), ".csv",
                       sep = "_"))
  
  rr2gt <- tibble::tibble(defect = as.character(), air = as.character(), 
                          lags = as.numeric(), lag = as.character(), 
                          rr = as.numeric(), cilow = as.numeric(), 
                          cihigh = as.numeric(), threshold = as.numeric())
  #aim 2 gt
  for (i in 1:length(nam)) {
    r <- calc_rr(data = d, defect = defect, lags = lags, 
                 model = model[[nam[i]]]$mgtcov, 
                 cb = model[[nam[i]]]$mgtcb1, 
                 measure = gsub("^[cfslp]+_|_lin__.+", "", nam[i]))
    rr2gt <- dplyr::bind_rows(rr2gt, r)
  }
  
  write.csv(rr2gt, paste("data/model2gt_rr", defect, lags, Sys.Date(), ".csv",
                         sep = "_"))
  
  rr2gw <- tibble::tibble(defect = as.character(), air = as.character(), 
                          lags = as.numeric(), lag = as.character(), 
                          rr = as.numeric(), cilow = as.numeric(), 
                          cihigh = as.numeric(), threshold = as.numeric())
  #aim 2 gw
  for (i in 1:length(nam)) {
    r <- calc_rr(data = d, defect = defect, lags = lags, 
                 model = model[[nam[i]]]$mgwcov, 
                 cb = model[[nam[i]]]$mgwcb1, 
                 measure = gsub("^[cfslp]+_|_lin__.+", "", nam[i]))
    rr2gw <- dplyr::bind_rows(rr2gw, r)
  }
  
  write.csv(rr2gw, paste("data/model2gw_rr", defect, lags, Sys.Date(), ".csv",
                         sep = "_"))
  
  
  # draw plots ----
  p0 <- list()
  p <- draw_forest_wrapper(data = rr1, defect_abbr = defect, lags = lags, 
                           defect_name = defect_name, green = "")
  p0$forest = grDevices::recordPlot(ggpubr::as_ggplot(p))
  p <- draw_forest_wrapper(data = rr2gt, defect_abbr = defect, lags = lags, 
                           defect_name = defect_name, green = "gt")
  p0$gt = grDevices::recordPlot(ggpubr::as_ggplot(p))
  p <- draw_forest_wrapper(data = rr2gw, defect_abbr = defect, lags = lags, 
                           defect_name = defect_name, green = "gw")
  p0$gw = grDevices::recordPlot(ggpubr::as_ggplot(p))
  
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
  plotfile <- paste("images/crossplots", defect, lags, Sys.Date(), 
                    ".pdf", sep = "_")
  grDevices::pdf(plotfile, onefile=TRUE, width = 10, height = 7)
  for (myplot in plots) grDevices::replayPlot(myplot)
  grDevices::dev.off() # need to close pdf file
  
}
