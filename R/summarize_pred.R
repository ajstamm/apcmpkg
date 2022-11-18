#' Summarize predictor details
#'
#' @param pred      Prediction model.
#' @param threshold Centering value used in the prediction.
#' @param air       Air pollutant.
#' @param lbl       Label for the model.
#'
#' @description
#' This function extracts parameters of the prediction model to be used for
#' assessment and plotting.
#'
#'

summarize_pred <- function(pred, threshold, air, lbl) {
  # estimated effects at each lag for given measure value
  r <- data.frame(rr = t(pred$matRRfit), cilow = t(pred$matRRlow),
                  cihigh = t(pred$matRRhigh))
  names(r) <- c("rr", "cilow", "cihigh")
  r$lag <- rownames(r)
  # estimated cumulative effect of all lags
  r <- rbind(r, data.frame(rr = pred$allRRfit, cilow = pred$allRRlow,
                           cihigh = pred$allRRhigh, lag = "cum"))
  r$model <- lbl
  r$threshold <- threshold
  r$green <- "grass_trees"
  r$air <- air

  return(r)
}
