#' Determining the cross-basis settings to use
#'
#' @param data     Dataset.
#' @param argvar   Shape of relationship between endpoint exposure and outcome.
#'                 Value entered for `argvar` in the cross-basis formula.
#' @param arglag   Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#' @param knots    Degrees of freedom for the `arglag` argument, if any.
#' @param startlag Maximum number of lags to include in th model.
#'
#' @description
#' This function determines the number of lags that should be used in the
#' cross-basis model. It begins with the maximum number of interest
#' (the startlag option). Each time a cross-basis is created, the final lag
#' is evaluated using a simple unadjusted model to determine significance at
#' alpha = 0.05 and alpha = 0.10. If the final lag is not significant at one
#' of these alphas, the model is run again with the oldest lag removed.
#'
#' The output includes the settings for the final cross-basis selected,
#' with the final lag being the oldest with significance at alpha = 0.05 if
#' applicable or, if none have significance at alpha = 0.05, the oldest final
#' lag with significance at alpha = 0.10 is retained. If no final lags have
#' significance at alpha = 0.05 or 0.10, the smallest possible cross-basis is
#' retained, with the number of lags being 1 plus the number of degrees of
#' freedom. For linear models, one lag was retained.
#'
#'
#' @export
#'


cb_diagnostics <- function(data, argvar = "lin", arglag = "lin", knots = NULL,
                           startlag = 16) {
  defect <- c("cf", "cp", "clp", "cs")
  measure <- c("mean", "peak")
  pollutant <- c("pm", "o3")
  mlist <- list()

  if (argvar == "lin" & arglag == "lin") knots <- 0

  for (i in 1:length(defect)) {
    d <- data |> dplyr::filter(!!dplyr::sym("set") == defect[i])
    if (nrow(d) > 0) {
      for (j in 1:length(measure)) {
        for (k in 1:length(pollutant)) {
          for (h in knots) {
            lag <- startlag
            lbl <- names(d)[grepl(paste(pollutant[k], measure[j], sep = "_"),
                                  names(d))][1:(startlag + 1)]
            lbl <- rev(lbl) # most recent first
            model <- list(mbcb = NULL, mccb = NULL, mbasic = NULL, mcov = NULL,
                          mblag = 0, mclag = 0, mbaic = 99999, mcaic = 99999,
                          argvar = argvar, arglag = arglag)
            while (lag > h) {
              # crossbasis ----
              t <- as.matrix(d[, lbl[1:(lag + 1)]], ncol = (lag + 1), byrow = FALSE)
              if (argvar == "lin") {
                if (arglag == "lin") {
                  cb <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                   arglag = list(fun = "lin"))
                } else if (arglag == "poly") {
                  model$arglag <- paste0("poly", h)
                  cb <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                   arglag = list(fun = "poly", degree = h))
                } else if (arglag == "ns") {
                  model$arglag <- paste0("ns", h)
                  cb <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                   arglag = list(fun = "ns", knots = h))
                }
              }
              if (arglag == "lin") {
                if (argvar == "poly") {
                  model$argvar <- paste0("poly", h)
                  cb <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "poly",
                                                               degree = h),
                                   arglag = list(fun = "lin"))
                } else if (argvar == "ns") {
                  model$argvar <- paste0("ns", h)
                  cb <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "ns",
                                                               knots = h),
                                   arglag = list(fun = "lin"))
                }
              }

              mb <- stats::glm(case ~ cb, family = stats::binomial(), d)
              mbaic <- stats::extractAIC(mb)[2]
              mc <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                                      cseason_cat + cb,
                        family = stats::binomial(), d)
              mcaic <- stats::extractAIC(mc)[2]

              if (mbaic < model$mbaic) {
                model$mbasic <- mb
                model$mbcb <- cb
                model$mblag <- lag
                model$mbaic <- mbaic
              }
              if (mcaic < model$mcaic) {
                model$mcov <- mc
                model$mccb <- cb
                model$mclag <- lag
                model$mcaic <- mcaic
              }

              lag <- lag - 1
            }
            lbl <- paste(model$argvar, model$arglag, sep = "_")
            mlist[[defect[i]]][[pollutant[k]]][[measure[j]]][[lbl]] <- model
          }
        }
      }
      mlist[[defect[i]]]$data <- d
    }

  }
  return(mlist)
}
