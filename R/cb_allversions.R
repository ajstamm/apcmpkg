#' Running a simple cross-basis model with all lags
#'
#' @param data     Dataset.
#' @param argvar   Shape of relationship between endpoint exposure and outcome.
#'                 Value entered for `argvar` in the cross-basis formula.
#' @param arglag1   Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#' @param knots1    Degrees of freedom for the `arglag` argument, if any.
#' @param arglag2   Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#' @param knots2    Degrees of freedom for the `arglag` argument, if any.
#' @param greens   Vector of strings denoting green space variables to be
#'                 included, one at a time. Use NULL to exclude gren space
#'                 variables altogether.
#' @param lags     The number of lags to include in the model.
#'
#' @description
#' This function determines the number of lags that should be used in the
#' cross-basis model. It uses the number of lags of interest.
#' Each time a cross-basis is created, the final lag
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

cb_allversions <- function(data, argvar = "lin", arglag1 = "lin", knots1 = 0,
                           arglag2 = "lin", knots2 = 0, lags = 16,
                           greens = c("gt", "gw")) {
  defect <- unique(data$set)
  lbl <- names(data)[grepl(".\\d+_.._.+", names(data))]
  measure <- unique(gsub(".\\d+_.._", "", lbl))
  measure <- measure[measure %in% c("mean", "peak")]
  pollutant <- unique(gsub("^.\\d+_|_....$", "", lbl))
  pollutant <- pollutant[pollutant %in% c("pm", "o3")]
  mlist <- list()

  if (length(argvar) == 1) argvar <- rep(argvar, 4)
  if (length(arglag1) == 1) arglag1 <- rep(arglag1, 4)
  if (length(knots1) == 1) knots1 <- rep(knots1, 4)
  if (length(arglag2) == 1) arglag2 <- rep(arglag2, 4)
  if (length(knots2) == 1) knots2 <- rep(knots2, 4)

  argvar <- matrix(argvar, nrow = 2, byrow = TRUE)
  arglag1 <- matrix(arglag1, nrow = 2, byrow = TRUE)
  knots1 <- matrix(knots1, nrow = 2, byrow = TRUE)
  arglag2 <- matrix(arglag2, nrow = 2, byrow = TRUE)
  knots2 <- matrix(knots2, nrow = 2, byrow = TRUE)

  for (i in 1:length(defect)) {
    # df not applicable for linear relationships
    # run cross-bases for each defect separately
    d <- dplyr::filter(data, set == defect[i])
    if (nrow(d) > 0) {
      for (j in 1:length(measure)) {
        for (k in 1:length(pollutant)) {
          model <- list(argvar = argvar[k, j], arglag1 = arglag1[k, j],
                        arglag2 = NULL,
                        mbcb1 = NULL, mbcb2 = NULL, mbasic = NULL, mblag = 0,
                        mbaic = 99999, mbp = 1,
                        mccb1 = NULL, mccb2 = NULL, mcov = NULL, mclag = 0,
                        mcaic = 99999, mcp = 1,
                        mgtcb1 = NULL, mgtcb2 = NULL, mgtcov = NULL, mgtlag = 0,
                        mgtaic = 99999, mgtp = 1,
                        mgwcb1 = NULL, mgwcb2 = NULL, mgwcov = NULL, mgwlag = 0,
                        mgwaic = 99999, mgwp = 1)
          # crossbasis ----
          if (argvar[k, j] == "lin") {
            p <- if (!is.null(arglag2)) "o3" else pollutant[k]
            lbl <- names(d)[grepl(paste(p, measure[j], sep = "_"),
                                  names(d))][1:(lags + 1)]
            lbl <- rev(lbl) # most recent first
            t <- as.matrix(d[, lbl[1:(lags + 1)]], ncol = (lags + 1), byrow = FALSE)
            if (arglag1[k, j] == "lin") {
              knots1[k, j] <- 0
              cb1 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                     arglag = list(fun = "lin"))
            } else if (arglag1[k, j] == "poly") {
              model$arglag1 <- paste0("poly", knots1[k, j])
              cb1 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                     arglag = list(fun = "poly", degree = knots1[k, j]))
            } else if (arglag1[k, j] == "ns") {
              model$arglag1 <- paste0("ns", knots1[k, j])
              cb1 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                     arglag = list(fun = "ns", knots = knots1[k, j]))
            }
            if (!is.null(arglag2)) {
              p <- "pm"
              lbl <- names(d)[grepl(paste(p, measure[j], sep = "_"),
                                    names(d))][1:(lags + 1)]
              lbl <- rev(lbl) # most recent first
              t <- as.matrix(d[, lbl[1:(lags + 1)]], ncol = (lags + 1), byrow = FALSE)
              if (arglag2[k, j] == "lin") {
                model$arglag2 <- "lin"
                cb2 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                       arglag = list(fun = "lin"))
              } else if (arglag2[k, j] == "poly") {
                model$arglag2 <- paste0("poly", knots2[k, j])
                cb2 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                       arglag = list(fun = "poly", degree = knots2[k, j]))
              } else if (arglag2[k, j] == "ns") {
                model$arglag2 <- paste0("ns", knots2[k, j])
                cb2 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "lin"),
                                       arglag = list(fun = "ns", knots = knots2[k, j]))
              }
            }
          }
          if (arglag1[k, j] == "lin") {
            if (argvar[k, j] == "poly") {
              model$argvar <- paste0("poly", knots1[k, j])
              cb1 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "poly",
                                                                  degree = knots1[k, j]),
                                     arglag = list(fun = "lin"))
            } else if (argvar[k, j] == "ns") {
              model$argvar <- paste0("ns", knots1[k, j])
              cb1 <- dlnm::crossbasis(t, lag = lags, argvar = list(fun = "ns",
                                                                  knots = knots1[k, j]),
                                     arglag = list(fun = "lin"))
            }
          }

          # evaluation models ----
          # unadjusted
          if (is.null(arglag2)) {
            mb <- stats::glm(case ~ cb1, family = stats::binomial(), d)
          } else {
            mb <- stats::glm(case ~ cb1 + cb2, family = stats::binomial(), d)
          }
          mbaic <- stats::extractAIC(mb)[2]

          # adjusted
          if (is.null(arglag2)) {
            mc <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                               cseason_cat + cb1,
                           family = stats::binomial(), d)
          } else {
            mc <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                               cseason_cat + cb1 + cb2,
                             family = stats::binomial(), d)
          }
          mcaic <- stats::extractAIC(mc)[2]

          # green space
          if ("gt" %in% greens) {
            if (is.null(arglag2)) {
              mgt <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                                  cseason_cat + grass_trees + cb1,
                                family = stats::binomial(), d)
            } else {
              mgt <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                                  cseason_cat + grass_trees + cb1 + cb2,
                                family = stats::binomial(), d)
            }
            mgtaic <- stats::extractAIC(mgt)[2] # adjusted
          }
          if ("gw" %in% greens) {
            if (is.null(arglag2)) {
              mgw <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                                  cseason_cat + green_water + cb1,
                                family = stats::binomial(), d)
            } else {
              mgw <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                                  cseason_cat + green_water + cb1 + cb2,
                                family = stats::binomial(), d)
            }
            mgwaic <- stats::extractAIC(mgw)[2] # adjusted
          }

          # evaluating the lag
          # basic model
          x <- broom::tidy(mb)
          x <- x[nrow(x), ]

          model$mbasic <- mb
          model$mbcb1 <- cb1
          if (!is.null(arglag2)) model$mbcb2 <- cb2
          model$mblag <- lags
          model$mbaic <- mbaic
          model$mbp <- x$p.value

          # covariate model
          y <- broom::tidy(mc)
          y <- y[nrow(y), ]

          model$mcov <- mc
          model$mccb1 <- cb1
          if (!is.null(arglag2)) model$mccb2 <- cb2
          model$mclag <- lags
          model$mcaic <- mcaic
          model$mcp <- y$p.value

          # green space models
          if ("gt" %in% greens) {
            y <- broom::tidy(mgt)
            y <- y[nrow(y), ]

            model$mgtcov <- mgt
            model$mgtcb1 <- cb1
            if (!is.null(arglag2)) model$mgtcb2 <- cb2
            model$mgtlag <- lag
            model$mgtic <- mgtaic
            model$mgtp <- min(y$p.value)
          }
          if ("gw" %in% greens) {
            y <- broom::tidy(mgw)
            y <- y[nrow(y), ]

            model$mgwcov <- mgt
            model$mgwcb1 <- cb1
            if (!is.null(arglag2)) model$mgwcb2 <- cb2
            model$mgwlag <- lag
            model$mgwic <- mgtaic
            model$mgwp <- min(y$p.value)
          }

          # final output
          lbl <- paste(defect[i], pollutant[k], measure[j],
                       model$argvar, model$arglag, lags, sep = "_")
          mlist[[lbl]] <- model
        }
      }
      mlist[[paste0(defect[i], "_data")]] <- d
    }
  }
  return(mlist)
}
