#' Create a series of diagnostic cross-basis plots
#'
#' @param bd      String denoting birth defect
#' @param air     String denoting air pollutant
#' @param measure String denoting type of pollutant measure
#' @param model   String denoting type of cross-basis model
#' @param list    List object containing the series of models
#'
#' @description
#' This function reads in a list produced by `cb_signif` or `cb_diagnostics`
#' and returns a series of plots, including:
#'
#' * residual plot overlaid best fit line
#' * lag CI plot
#' * cumulative CI plot
#' * plot with discrete lag CIs
#' * 3D graph of risk ratio, lags, and pollutant levels
#'
#' The code for these plots is adapted from the code provided in the `dlnm`
#' package vignette.
#'
# need example

cross_plot <- function(bd, air, measure, model, list) {
  cb <- list[[bd]][[air]][[measure]][[model]]$mccb
  m <- list[[bd]][[air]][[measure]][[model]]$mcov
  lag <- list[[bd]][[air]][[measure]][[model]]$mclag
  d <- list[[bd]]$data
  l <- rev(names(d)[grepl(paste(air, measure, sep = "_"),
                          names(d))]) # most recent first

  # plotvar & centering should be in range of pollutant values
  # and must be included in the atvals vector
  if (air=="pm" & measure=="mean") {
    centre <- 20 # Q1
    atvals <- 1:37
  } else if (air=="pm") { # peak
    centre <- 30 # Q1
    atvals <- 3:81
  } else if (measure=="mean") { # O3
    centre <- 60 # Q1
    atvals <- 8:93
  } else { # O3 peak
    centre <- 80 # Q1
    atvals <- 15:126
  }
  plotvar <- centre + 10 # 10-unit increase

  plots <- list()

  # create plot object
  grDevices::dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  grDevices::dev.control('enable') # enable display list
  graphics::par(mar = c(4, 2, 2, 1), mgp = c(3, 1, 0), xpd = TRUE)

  # CI bars chart
  cp <- dlnm::crosspred(cb, m, cen = centre, at = atvals, bylag = 1)
  title <- paste(bd, air, measure, "single-pollutant CI lines")
  plot(cp, "slices", ci="bars", type="p", col=2, pch=19, var = plotvar,
       ci.level=0.95, main=title, xlim = c(0, lag), ylab = "RR")
  plots$singleline <- grDevices::recordPlot()
  # cumulative charts
  cp <- dlnm::crosspred(cb, m, cumul=TRUE, cen = centre, at = atvals, bylag = 0.1)
  title <- paste(bd, air, measure, "Lags CI curve")
  plot(cp, "slices", var=plotvar, col=3, ylab="RR", xlim = c(0, lag),
       ci.arg=list(density=15,lwd=2), main=title)
  plots$lagsci <- grDevices::recordPlot()
  title <- paste(bd, air, measure, "Cumulative CI curve")
  plot(cp, "slices", var=plotvar, col=2, cumul=TRUE, ylab="Cumulative RR",
       main=title)
  plots$cumci <- grDevices::recordPlot()
  title <- paste(bd, air, measure, "3D graph")
  plot(cp, xlab=air, zlab="RR", theta=200, phi=40, lphi=30, main=title)
  plots$plot3d <- grDevices::recordPlot()
  # plot(cp, "contour", xlab="O3", key.title=title("RR"),
  #      plot.title=title("Contour plot", xlab="O3", ylab="Lag"))
  # diagnostics
  cb <- list[[bd]][[air]][[measure]][[model]]$mbcb
  m <- list[[bd]][[air]][[measure]][[model]]$mbasic
  lag <- list[[bd]][[air]][[measure]][[model]]$mblag
  b <- d %>% select(!!sym(l[lag + 1])) %>% unlist()
  title <- paste(bd, air, measure, "residuals")
  plot(b, resid(m), main = title, xlab = air, ylab = "residuals")
  graphics::abline(stats::lm(stats::resid(m) ~ b))
  plots$resid <- grDevices::recordPlot()

  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0))
  grDevices::dev.off()
  return(plots)
}
