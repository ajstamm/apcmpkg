#' Extract model risk ratios
#'
#' @param models  List of settings for multiple models.
#' @param buffers Green space buffers.
#'
#' @description
#' This function reads in a list of models and loops through them to extract
#' risk ratios and other details.
#' It returns a tibble with the details about each model.
#'
#' @export
#'
# checking by buffer because I kept erroring ...

rr_by_buffer <- function(models, buffers) {
  r <- list(base = tibble::tibble(model = as.character(),
                                  model_type = as.character(),
                          defect = as.character(), measure = as.character(),
                          air = as.character(), lags = as.numeric(),
                          buffer = as.character(), green = as.character(),
                          threshold = as.numeric(), lag = as.character(),
                          rr = as.numeric(), cilow = as.numeric(),
                          cihigh = as.numeric(), sig = as.logical()))
  for (i in buffers) {
    n <- names(models[[i]])[grepl("_", names(models[[i]]))]
    buffer <- names(models)[i]
    for (j in 1:length(n)) {
      bd <- gsub("_.+", "", n[j])
      lbl <- paste(n[j], buffer, sep = "_")
      t <- extract_rr(model = models[[buffer]][[n[j]]], label = lbl,
                      data = models[[buffer]][[bd]]$data)
      r[[lbl]] <- t
      rm(bd, lbl, t)
    }

  }

  rr <- dplyr::bind_rows(r)
  return(rr)
}
