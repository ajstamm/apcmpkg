# run models for aim 3
# I think this is specific to two-pollutant, but too tired to care right now


run_models <- function(settings, data, buffers = NULL) {
  mdls <- list()
  if (is.null(buffers)) buffers <- c(50, seq(from = 100, to = 500, by = 100))
  for (i in 1:nrow(settings)) {
    for (j in 1:length(buffers)) {
      d <- dplyr::filter(data, set == settings$bd[i]) |>
        dplyr::mutate(grass = !!sym(paste0("b", buffers[j], "_grass")),
                      trees = !!sym(paste0("b", buffers[j], "_trees")))
      medians <- list(grass = median(d$grass, na.rm = TRUE),
                      trees = median(d$trees, na.rm = TRUE))
      d <- dplyr::mutate(d, grass_trees = ifelse(grass > medians$grass &
                                                   trees > medians$trees, "GT",
                                                 ifelse(grass > medians$grass, "Gt",
                                                        ifelse(trees > medians$trees, "gT",
                                                               "gt")))) |>
        dplyr:: select(set, case, edu_cat, inc_cat, tobacco_cat, cseason_cat,
                       grass_trees, dplyr::contains(settings$Measure[i]))
        # "contains()" may be from tidyselect; in case of compile error

      ko3 <- as.numeric(substr(settings$o3[i], 5, 5))
      if (!is.finite(ko3)) ko3 <- 0
      kpm <- as.numeric(substr(settings$pm[i], 5, 5))
      if (!is.finite(kpm)) kpm <- 0
      ao3 <- substr(settings$o3[i], 1, 4)
      apm <- substr(settings$pm[i], 1, 4)

      # something messed up in model saving here
      # only two defects, and apparently misnamed
      # argvar = "lin"; arglag1 = ao3; knots1 = ko3; arglag2 = apm; knots2 = kpm
      # startlag = settings$lags[i]; greens = "gt"
      t <- cb_signif_green(d, argvar = "lin", arglag1 = ao3, knots1 = ko3,
                           arglag2 = apm, knots2 = kpm, greens = "gt",
                           startlag = settings$lags[i])


      # lbl <- paste(settings$bd[i], settings$Measure[i], settings$lags[i],
      #              paste("b", buffers[j], sep = ""), sep = "_")
      # lbl <- gsub("[^a-z_0-6]", "", tolower(lbl))
      lbl <- paste("b", buffers[j], sep = "")
      mdls[[lbl]][[names(t)[1]]] <- t[[1]]
      mdls[[lbl]][[names(t)[2]]] <- t[[2]]
    }
  }
  return(mdls)
}
