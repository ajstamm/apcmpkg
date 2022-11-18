#' Determining number of lags in the cross-basis model with green space
#'
#' @param data     Dataset.
#' @param argvar   Shape of relationship between endpoint exposure and outcome.
#'                 Value entered for `argvar` in the cross-basis formula.
#' @param arglag1  Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#' @param knots1   Degrees of freedom for the `arglag1` argument, if any.
#' @param arglag2  Shape of relationship between lagged exposure and outcome.
#'                 Value entered for `arglag` in the cross-basis formula.
#'                 This setting is optional.
#' @param knots2   Degrees of freedom for the `arglag2` argument, if any.
#'                 This setting is optional.
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
#' If two cross-bases are used in the model, the same number of lags is used
#' for both and significance is determined if either one is significant.
#'
#'
# check cross-basis with green space
# saves cross-basis and model parameters

cb_signif_green <- function(data, argvar = "lin", arglag1 = "lin", knots1 = NULL,
                            arglag2 = NULL, knots2 = NULL, startlag = 16,
                            greens = c("gt", "gw")) {
  defect <- unique(data$set)
  lbl <- names(data)[grepl(".\\d+_.._.+", names(data))]
  measure <- unique(gsub(".\\d+_.._", "", lbl))
  pollutant <- unique(gsub("^.\\d+_|_....$", "", lbl))
  nlist <- list()

  if (is.null(knots1) | arglag1 == "lin") knots1 <- 0
  if (is.null(knots2) | arglag2 == "lin") knots2 <- 0

  for (i in 1:length(defect)) {
    d <- data %>% filter(set == defect[i])
    if (nrow(d) > 0) {
      for (j in 1:length(measure)) {
        for (k in 1:length(pollutant)) {
            lag <- as.numeric(startlag)
            model <- list(mbcb1 = NULL, mbcb2 = NULL,mbasic = NULL, mblag = 0,
                          mbaic = 99999, mbp = 1, mgtcb1 = NULL, mgtcb2 = NULL,
                          mgtcov = NULL, mgtlag = 0, mgtaic = 99999, mgtp = 1,
                          mgwcb1 = NULL, mgwcb2 = NULL, mgwcov = NULL, mgwp = 1,
                          mgwlag = 0, mgwaic = 99999, argvar = argvar,
                          arglag1 = arglag1, arglag2 = arglag2)
            while (lag > max(knots1, knots2)) {
              # crossbasis ----
              if (argvar == "lin") {
                p <- if (!is.null(arglag2)) "o3" else pollutant[k]
                lbl <- names(d)[grepl(paste(p, measure[j], sep = "_"),
                                      names(d))][1:(as.numeric(startlag) + 1)]
                lbl <- rev(lbl) # most recent first
                t <- as.matrix(d[, lbl[1:(lag + 1)]], ncol = (lag + 1), byrow = FALSE)
                if (arglag1 == "lin") {
                  cb1 <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                    arglag = list(fun = "lin"))
                } else if (arglag1 == "poly") {
                  model$arglag1 <- paste0("poly", knots1)
                  cb1 <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                   arglag = list(fun = "poly", degree = knots1))
                }
                if (!is.null(arglag2)) {
                  p <- "pm"
                  lbl <- names(d)[grepl(paste(p, measure[j], sep = "_"),
                                        names(d))][1:(as.numeric(startlag) + 1)]
                  lbl <- rev(lbl) # most recent first
                  t <- as.matrix(d[, lbl[1:(lag + 1)]], ncol = (lag + 1), byrow = FALSE)
                  if (arglag2 == "lin") {
                    cb2 <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                      arglag = list(fun = "lin"))
                  } else if (arglag2 == "poly") {
                    model$arglag2 <- paste0("poly", knots2)
                    cb2 <- dlnm::crossbasis(t, lag = lag, argvar = list(fun = "lin"),
                                      arglag = list(fun = "poly", degree = knots2))
                  }
                }
              }

              # basic model ----
              if (is.null(arglag2)) {
                mb <- stats::glm(case ~ cb1, family = stats::binomial(), d)
              } else {
                mb <- stats::glm(case ~ cb1 + cb2, family = stats::binomial(), d)
              }
              mbaic <- extractAIC(mb)[2]
              # evaluating the lag
              x <- broom::tidy(mb) |>
                dplyr::filter(grepl("^cb", term)) |>
                dplyr::mutate(var = substr(term, 1, 3)) |>
                dplyr::group_by(var) |> dplyr::slice_tail()
              if (sum(!is.na(x$p.value)) > 0) {
                if ((sum(x$p.value < 0.05) > 1 & model$mbp > 0.05) |
                    (sum(x$p.value < 0.1) > 1 & model$mbp > 0.1) |
                    (lag == max(knots1, knots2) + 1 & model$mbp > 0.1)) {
                  model$mbasic <- mb
                  # cb <- x$var[x$p.value == min(x$p.value)]
                  # model$mbcb <- rlang::eval_bare(rlang::sym(cb))
                  model$mbcb1 <- cb1
                  model$mbcb2 <- cb2
                  model$mblag <- lag
                  model$mbaic <- mbaic
                  model$mbp <- min(x$p.value)
                  if (!is.null(arglag2)) model$mbcb2 <- cb2
                }
              }
              # grass-trees ----
              if ("gt" %in% greens) {
                if (is.null(arglag2)) {
                  mgt <- stats::glm(case ~ edu_cat + inc_cat + tobacco_cat +
                               cseason_cat + grass_trees + cb1,
                             family = stats::binomial(), d)
                } else {
                  mgt <- stats::glm(case ~ edu_cat + tobacco_cat +
                               inc_cat + cseason_cat + grass_trees + cb1 + cb2,
                             family = stats::binomial(), d)
                }
                mgtaic <- stats::extractAIC(mgt)[2]
                y <- broom::tidy(mgt) |>
                  dplyr::filter(grepl("^cb", term)) |>
                  dplyr::mutate(var = substr(term, 1, 3)) |>
                  dplyr::group_by(var) |> dplyr::slice_tail()
                # even though cov doesn't matter if basic fails ...
                if (sum(!is.na(y$p.value)) > 0) {
                  if ((sum(y$p.value < 0.05) > 1 & model$mgtp > 0.05) |
                      (sum(y$p.value < 0.1) > 1 & model$mgtp > 0.1) |
                      (lag == max(knots1, knots2) + 1 & model$mgtp > 0.1)) {
                    model$mgtcov <- mgt
                    model$mgtcb1 <- cb1
                    model$mgtcb2 <- cb2
                    model$mgtlag <- lag
                    model$mgtic <- mgtaic
                    model$mgtp <- min(y$p.value)
                    if (!is.null(arglag2))  model$mgtcb2 <- cb2
                  }
                }
              }
              # green-water ----
              if ("gw" %in% greens) {
                if (is.null(arglag2)) {
                  mgw <- glm(case ~ cb1 + edu_cat + inc_cat + tobacco_cat +
                               cseason_cat + green_water,
                             family = stats::binomial(), d)
                } else {
                  mgw <- glm(case ~ cb1 + cb2 + edu_cat + tobacco_cat +
                               inc_cat + cseason_cat + green_water,
                             family = stats::binomial(), d)
                }
                mgwaic <- stats::extractAIC(mgw)[2]
                z <- broom::tidy(mgw) |>
                  dplyr::filter(grepl("^cb", term)) |>
                  dplyr::mutate(var = substr(term, 1, 3)) |>
                  dplyr::group_by(var) |> dplyr::slice_tail()
                if (sum(!is.na(z$p.value)) > 0) {
                  if ((sum(z$p.value < 0.05) > 1 & model$mgwp > 0.05) |
                      (sum(z$p.value < 0.1) > 1 & model$mgwp > 0.1) |
                      (lag == max(knots1, knots2) + 1 & model$mgwp > 0.1)) {
                    model$mgwcov <- mgw
                    model$mgwcb1 <- cb1
                    model$mgwcb2 <- cb2
                    model$mgwlag <- lag
                    model$mgwaic <- mgwaic
                    model$mgwp <- min(z$p.value)
                    if (!is.null(arglag2)) model$mgwcb2 <- cb2
                  }
                }
              }

              lag <- lag - 1
            }
            lbl <- paste(model$argvar, model$arglag1, model$arglag2, sep = "_")
            nlist[[paste(defect[i], measure[j], startlag, sep = "_")]] <- model
        }
      }
      nlist[[defect[i]]]$data <- d
    }

  }
  return(nlist)
}
