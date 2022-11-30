#' Create a series of diagnostic cross-basis plots
#'
#' @param bd      String denoting birth defect
#' @param air     String denoting air pollutant
#' @param measure String denoting type of pollutant measure
#' @param model   String denoting type of cross-basis model
#' @param list    List object containing the series of models
#' @param centre  The threshold or centering value
#' @param atvals  The air pollutant concentrations to predict
#' @param plotvar The unit increase of interest to calculate risk ratios
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
#' @export
# need example

cross_plot <- function(bd, air, measure, model, list,
                       centre = NULL, atvals = NULL, plotvar = NULL) {
  cb <- list[[bd]][[air]][[measure]][[model]]$mccb
  mc <- list[[bd]][[air]][[measure]][[model]]$mcov
  lag <- list[[bd]][[air]][[measure]][[model]]$mclag
  d <- list[[bd]]$data
  l <- rev(names(d)[grepl(paste(air, measure, sep = "_"),
                          names(d))]) # most recent first

  # plotvar & centering should be in range of pollutant values
  # and must be included in the atvals vector
  if (is.null(centre)) {
    if (air=="pm" & measure=="mean") {
      centre <- 20 # Q1
    } else if (air=="pm") { # peak
      centre <- 30 # Q1
    } else if (measure=="mean") { # O3
      centre <- 60 # Q1
    } else { # O3 peak
      centre <- 80 # Q1
    }
  }
  if (is.null(plotvar)) plotvar <- centre + 10 # 10-unit increase
  if (is.null(atvals)) {
    t <- d[, l]
    atvals <- c(floor(min(t)):ceiling(max(t)), centre, plotvar)
  }

  plots <- list()

  # create plot object
  grDevices::dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  grDevices::dev.control('enable') # enable display list
  graphics::par(mar = c(4, 2, 2, 1), mgp = c(3, 1, 0), xpd = TRUE)

  # CI bars chart
  cp <- dlnm::crosspred(cb, mc, cen = centre, at = atvals, bylag = 1)
  title <- paste(bd, air, measure, "single-pollutant CI lines")
  plot(cp, "slices", ci="bars", type="p", col=2, pch=19, var = plotvar,
       ci.level=0.95, main=title, xlim = c(0, lag), ylab = "RR")
  plots$singleline <- grDevices::recordPlot()
  # cumulative charts
  cp <- dlnm::crosspred(cb, mc, cumul=TRUE, cen = centre, at = atvals, bylag = 0.1)
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
  b <- d |> dplyr::select(!!dplyr::sym(l[lag + 1])) |> unlist()
  title <- paste(bd, air, measure, "residuals")
  plot(b, stats::resid(m), main = title, xlab = air, ylab = "residuals")
  graphics::abline(stats::lm(stats::resid(m) ~ b))
  plots$resid <- grDevices::recordPlot()

  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0))
  grDevices::dev.off()
  return(plots)
}
