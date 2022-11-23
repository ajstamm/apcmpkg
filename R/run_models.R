# run models for aim 3
# I think this is specific to two-pollutant, but too tired to care right now

#' Run the models
#'
#' @param settings Model settings table.
#' @param data     Dataset to model.
#' @param buffers  Green space buffers, if relevant.
#'
#' This function runs the multi-pollutant models for Aim 3.
#'
#' @export
#'
#'



run_models <- function(settings, data, buffers = NULL) {
  mdls <- list()
  if (is.null(buffers)) buffers <- c(50, seq(from = 100, to = 500, by = 100))
  for (i in 1:nrow(settings)) {
    for (j in 1:length(buffers)) {
      d <- dplyr::filter(data, !!dplyr::sym("set") == settings$bd[i]) |>
        dplyr::mutate(grass = !!dplyr::sym(paste0("b", buffers[j], "_grass")),
                      trees = !!dplyr::sym(paste0("b", buffers[j], "_trees")))
      medians <- list(grass = stats::median(d$grass, na.rm = TRUE),
                      trees = stats::median(d$trees, na.rm = TRUE))
      d <- dplyr::mutate(d, grass_trees = ifelse(!!dplyr::sym("grass") > medians$grass &
                                                 !!dplyr::sym("trees") > medians$trees, "GT",
                                          ifelse(!!dplyr::sym("grass") > medians$grass, "Gt",
                                          ifelse(!!dplyr::sym("trees") > medians$trees, "gT",
                                                   "gt")))) |>
        dplyr:: select(!!dplyr::sym("set"), !!dplyr::sym("case"), !!dplyr::sym("edu_cat"), !!dplyr::sym("inc_cat"),
                       !!dplyr::sym("tobacco_cat"), !!dplyr::sym("cseason_cat"),
                       !!dplyr::sym("grass_trees"), dplyr::contains(settings$Measure[i]))
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
