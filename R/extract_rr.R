#' Extract model risk ratios
#'
#' @param model  Model settings.
#' @param label  Model label (name from a model list).
#' @param data   Dataset.
#'
#' @description
#' This function extracts risk ratios from two different cross-basis objects
#' when both are used in the same model.
#'
#' At the moment, cross-prediction settings (notably centering values) are
#' hard-coded, but that may be changed later.
#'
#' Note to self: I modified this at some point - make sure it still works when
#' only one cross-basis is used.
#'
#' @export
#'

# extract risk ratios for aim 3

extract_rr <- function(model, label, data) {
  # set up ----
  msr <- gsub("^[a-z]+_|\\d+|_|b", "", label)
  lags <- as.numeric(gsub("_", "", substr(label, 9, 11)))
  r <- list()

  if (grepl("mean", msr)) {
    pcentre <- 20 # Q1
    patvals <- 1:37
    ocentre <- 60 # Q1
    oatvals <- 8:93
  } else { # peak
    pcentre <- 30 # Q1
    patvals <- 3:81
    ocentre <- 80 # Q1
    oatvals <- 15:126
  }
  pplotvar <- pcentre + 10
  oplotvar <- ocentre + 10


  # unadjusted model ----
  cb1 <- model$mbcb1
  cb2 <- model$mbcb2
  m <- model$mbasic
  lag <- model$mblag

  p1 <- dlnm::crosspred(cb1, m, at = oplotvar, cen = ocentre)
  p2 <- dlnm::crosspred(cb2, m, at = pplotvar, cen = pcentre)
  r[["basic"]] <- dplyr::bind_rows(
    summarize_pred(p1, threshold = ocentre, air = "o3", lbl = label),
    summarize_pred(p2, threshold = pcentre, air = "pm", lbl = label)
  ) |> dplyr::mutate(model_type = "unadjusted")


  # adjusted model ----
  cb1 <- model$mgtcb1
  cb2 <- model$mgtcb2
  m <- model$mgtcov
  lag <- model$mgtlag

  p1 <- dlnm::crosspred(cb1, m, at = oplotvar, cen = ocentre)
  p2 <- dlnm::crosspred(cb2, m, at = pplotvar, cen = pcentre)
  r[["cov"]] <- dplyr::bind_rows(
    summarize_pred(p1, threshold = ocentre, air = "o3", lbl = label),
    summarize_pred(p2, threshold = pcentre, air = "pm", lbl = label)
  ) |> dplyr::mutate(model_type = "adjusted")

  # combine & return ----
  f <- dplyr::bind_rows(r) |>
    dplyr::mutate(measure = msr, lags = lags,
                  buffer = gsub(".+_", "", model),
                  defect = gsub("_.+", "", model),
                  sig = !!dplyr::sym("cihigh") < 1 | !!dplyr::sym("cilow") > 1)

  return(f)
}
