#' Table of risk ratios by lag
#'
#' This function draws a catch-all table for displaying risk ratios by lag
#' for one birth defect and green space set of risk ratios.
#'
#' @param bd           String denoting birth defect as coded in the risk ratios
#'                     dataset.
#' @param my_caption   String denoting table title.
#' @param file         CSV file containing the risk ratios table.
#' @param file_compare File of risk ratios to compare to the table under
#'                     evaluation.
#
# temporary function name - might work for all 3 aims

bd_rr2comp1_300m <- function(bd, my_caption, file, file_compare = NULL) {

  t <- readr::read_csv(file) |> dplyr::filter(!lag == "cum", defect == bd) |>
    dplyr::mutate(cil = sprintf("%1.2f", cilow),
                  cih = sprintf("%1.2f", cihigh),
                  flag = cilow > 1 | cihigh < 1, # add bold code if flag
                  measure = gsub(".+[ _]", "", air),
                  new_air = ifelse(grepl("pm", air), "pm", "o3"),
                  pollutant = paste("Weekly", measure, "concentration"),
                  new_lag = as.numeric(gsub("lag", "", lag)),
                  new_lag = ifelse(lags == 12, new_lag + 4, new_lag),
                  week = 12 - new_lag,
                  ci = paste0("(", cil, " - ", cih, ")"),
                  ci = ifelse(flag, paste0("\\textbf{", ci, "}"), ci),
                  rr_new = round(rr, digits = 2),
                  rr = sprintf("%1.2f", rr),
                  rr = ifelse(flag, paste0("\\textbf{", rr, "}"), rr))

  if (!is.null(file_compare)) {
    rr <- readr::read_csv(file_compare) |>
      dplyr::filter(!lag == "cum", defect == bd) |>
      dplyr::select(defect, air, lags, lag, rr) |>
      dplyr::rename(rr_old = rr)
    t <- dplyr::full_join(t, rr, by = c("defect", "air", "lags", "lag")) |>
      dplyr::mutate(rr_old = round(rr_old, digits = 2),
                    delta = ifelse(rr_new > rr_old, "\\uparrow",
                                   ifelse(rr_new < rr_old, "\\downarrow", "")),
                    rr = paste(delta, rr))
  }
  # see https://latex-tutorial.com/arrow-latex/
  t <- t |>
    dplyr::select(defect, pollutant, week, new_air, lags, rr, ci) |>
    dplyr::arrange(defect, pollutant, lags, week) |>
    tidyr::gather(key = "vals", value = "ests", rr:ci) |>
    dplyr::mutate(vals = paste(new_air, vals, lags, sep = "_")) |>
    dplyr::select(defect, pollutant, week, vals, ests) |>
    tidyr::spread(key = "vals", value = "ests") |>
    dplyr::select(pollutant, week, o3_rr_12, o3_ci_12, o3_rr_16, o3_ci_16,
                  pm_rr_12, pm_ci_12, pm_rr_16, pm_ci_16) |> dplyr::ungroup()
  t[is.na(t)] <- ""

  l <- unique(t$pollutant)
  b <- table(t$pollutant)

  foot <- "RR = risk ratio; CI = confidence interval; PM = particulate matter"
  if (!is.null(file_compare)) {
    if (grepl(1, file_compare)) {
      foot <- c(foot, paste("\\\\uparrow = RR is higher than in the original",
                            "model; \\\\downarrow = RR is lower than in the",
                            "original model"))
    } else {
      foot <- c(foot, paste("\\\\uparrow = RR is higher than in the",
                            "single-pollutant models; \\\\downarrow = RR is",
                            "lower than in the single-pollutant models"))
    }
  }
  if (grepl(1, file)) {
    foot <- c(foot, paste("Models were adjusted for maternal education level,",
                          "maternal smoking, tract-level median income, and "),
              "conception season. ")
  } else if (grepl(2, file)) {
    foot <- c(foot, paste("Models were adjusted for maternal education level,",
                          "maternal smoking, tract-level median income,"),
              "conception season, and green space. ")
  } else if (grepl(3, file)) {
    foot <- c(foot, paste("Models were adjusted for maternal education level,",
                          "maternal smoking, tract-level median income,"),
              "conception season, green space, ozone, and PM\\\\textsubscript{2.5}. ")
  }
  foot <- c(foot,
            "RR applies to a 10-unit increase from the previous week in the air pollutant (ppb for ozone and ",
            "µg/m\U00B3 for PM\U2082.\U2085) over two standard deviations above the mean.",
            "Week 0 refers to the week of conception. Week 12 refers to the end of the first trimester of pregnancy.")

  t |> dplyr::select(-pollutant) |>
    knitr::kable(booktabs = TRUE, format = "latex", escape = F, # longtable = T,
                 caption = my_caption,
                 col.names = c("Week", rep(c("RR", "(95\\% CI)"), 4)),
                 align = c("c", rep(c("r", "l"), 4))) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::column_spec(1, bold = FALSE, width = "3em") |> #
    # kableExtra::column_spec(c(2), bold = FALSE, width = "3em") |> #
    kableExtra::column_spec(c(2, 4, 6, 8), bold = FALSE, width = "3em") |> #
    kableExtra::column_spec(c(3, 5, 7, 9), bold = FALSE, width = "5em") |> #
    kableExtra::column_spec(c(2, 6), border_left = T) |> #
    kableExtra::group_rows(index = b) |> #
    kableExtra::add_header_above(c(rep(" ", 1), "12-week model" = 2,
                                   "16-week model" = 2, "12-week model" = 2, "16-week model" = 2)) |>
    kableExtra::add_header_above(c(rep(" ", 1), "Ozone" = 4,
                                   "PM\\\\textsubscript{2.5}" = 4), escape = F) |>
    kableExtra::kable_styling(
      latex_options = c("repeat_header"), # "striped", "hold_position",
      position = "center", font_size = 10, # full_width = TRUE,
      repeat_header_continued = "\\textit{(Continued on Next Page...)}") |>
    kableExtra::footnote(foot, escape = F)

}
