#' Create a basic residual plot
#'
#' @param bd      String denoting birth defect
#' @param air     String denoting air pollutant
#' @param measure String denoting type of pollutant measure
#' @param model   String denoting type of cross-basis model
#' @param list    List object containing the series of models
#'
#' This function reads in a list produced by `cb_signif` or `cb_diagnostics`
#' and returns a residual plot with basic, but correct, labels and an overlaid
#' best fit line.
#'
#' @export
#'

residual_plot <- function(bd, air, measure, model, list) {
  m <- list[[bd]][[air]][[measure]][[model]]$mbasic
  d <- list[[bd]]$data
  l <- rev(names(d)[grepl(paste(air, measure, sep = "_"),
                          names(d))]) # most recent first
  lag <- list[[bd]][[air]][[measure]][[model]]$mblag

  print(summary(m))
  x <- d |> dplyr::select(!!dplyr::sym(l[lag + 1])) |> unlist()
  plot(x, m$residuals, main = paste(bd, air, measure, model),
       xlab = air, ylab = "residuals")
  graphics::abline(stats::lm(m$residuals ~ x))
}
