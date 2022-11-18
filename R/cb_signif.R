#' Determining number of lags in the cross-basis model
#'
#' @param data     Dataset.
#' @param argvar   Shape of relationship between endpoint exposure and outcome.
#'                 Value entered for `argvar` in the cross-basis formula.
#' @param arglag   Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#' @param knots    Degrees of freedom for the `arglag` argument, if any.
#' @param startlag Maximum number of lags to include in the model.
#'
#' @description
#' This function determines the number of lags that should be used in the
#' cross-basis model. It begins with the maximum number of interest
#' (the startlag option). Each time a cross-basis is created, the final lag
#' is evaluated using unadjusted and adjusted models to determine significance
#' at alpha = 0.05 and alpha = 0.10. If the final lag is not significant at one
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
#'
#'

cb_signif <- function(data, argvar = "lin", arglag = "lin", knots = NULL,
                      startlag = 16) {
  defect <- unique(data$set)
  lbl <- names(data)[grepl(".\\d+_.._.+", names(data))]
  measure <- unique(gsub(".\\d+_.._", "", lbl))
  pollutant <- unique(gsub("^.\\d+_|_....$", "", lbl))
  mlist <- list()

  # df not applicable for linear relationships
  if (argvar == "lin" & arglag == "lin") knots <- 0

  for (i in 1:length(defect)) {
    # run cross-bases for each defect separately
    d <- data %>% filter(set == defect[i])
    if (nrow(d) > 0) {
      for (j in 1:length(measure)) {
        for (k in 1:length(pollutant)) {
          for (h in knots) {
            lag <- startlag
            lbl <- names(d)[grepl(paste(pollutant[k], measure[j], sep = "_"),
                                  names(d))][1:(startlag + 1)]
            lbl <- rev(lbl) # most recent week first
            model <- list(mbcb = NULL, mccb = NULL, mbasic = NULL, mcov = NULL,
                          mblag = 0, mclag = 0, mbaic = 99999, mcaic = 99999,
                          mbp = 1, mcp = 1, argvar = argvar, arglag = arglag)
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

              # evaluation models ----
              mb <- stats::glm(case ~ cb, family = binomial(), d)
              mbaic <- stats::extractAIC(mb)[2] # unadjusted
              mc <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat + cseason_cat + cb,
                        family = stats::binomial(), d)
              mcaic <- stats::extractAIC(mc)[2] # adjusted

              # evaluating the lag
              x <- broom::tidy(mb)
              y <- broom::tidy(mc)
              x <- x[nrow(x), ]
              y <- y[nrow(y), ]
              if (!is.na(x$p.value)) {
                if (x$p.value < 0.05 & model$mbp > 0.05) {
                  model$mbasic <- mb
                  model$mbcb <- cb
                  model$mblag <- lag
                  model$mbaic <- mbaic
                  model$mbp <- x$p.value
                } else if (x$p.value < 0.1 & model$mbp > 0.1) {
                  model$mbasic <- mb
                  model$mbcb <- cb
                  model$mblag <- lag
                  model$mbaic <- mbaic
                  model$mbp <- x$p.value
                } else if (lag == h + 1 & model$mbp > 0.1) {
                  model$mbasic <- mb
                  model$mbcb <- cb
                  model$mblag <- lag
                  model$mbaic <- mbaic
                  model$mbp <- x$p.value
                }
              }
              # even though cov doesn't matter if basic fails ...
              if (!is.na(y$p.value)) {
                if (y$p.value < 0.05 & model$mcp > 0.05) {
                  model$mcov <- mc
                  model$mccb <- cb
                  model$mclag <- lag
                  model$mcaic <- mcaic
                  model$mcp <- y$p.value
                } else if (y$p.value < 0.1 & model$mcp > 0.1) {
                  model$mcov <- mc
                  model$mccb <- cb
                  model$mclag <- lag
                  model$mcaic <- mcaic
                  model$mcp <- y$p.value
                } else if (lag == h + 1 & model$mcp > 0.1) {
                  model$mcov <- mc
                  model$mccb <- cb
                  model$mclag <- lag
                  model$mcaic <- mcaic
                  model$mcp <- y$p.value
                }
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
