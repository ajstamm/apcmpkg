# possibly also use for aim 2 cum table and a variant for aim 1 table(s)
bd_rr3_tables <- function(my_bd, my_caption, bold_rows = 0) {
  my_foot <- c(paste("Model was adjusted for O\\\\textsubscript{3},",
                     "PM\\\\textsubscript{2.5}, maternal education level,",
                     "maternal smoking, tract-level median income, "),
               paste("conception season, and presence of grasses or trees.",
                     "Values in bold were significant at $\\\\alpha = 0.05$."))

  if (grepl("cf", my_bd)) {
    my_foot <- c(my_foot, paste("Week 0 is the week of conception.",
                                "For clubfoot, only the 16-week model was run because limb formation"), "finishes around week 12.")
  } else {
    my_foot <- c(my_foot, "Week 0 is the week of conception.")
  }

  t <- readr::read_csv("data/model3cov_rrciwide_2022-08-26_all.csv") |>
    dplyr::filter(defect == my_bd, model_type == "adjusted", !lag == "cum") |>
    dplyr::mutate(model = gsub("_1._b.+", "", model),
                  o3 = ifelse(is.na(o3), "", o3),
                  pm = ifelse(is.na(pm), "", pm),
                  lag = as.numeric(gsub("lag", "", lag)),
                  week = ifelse(lags == 12, lag + 4, lag),
                  week = 12 - week,
                  buffer = as.numeric(gsub("b", "", buffer))) |>
    dplyr::select(defect, measure, buffer, lags, week, o3, pm)

  t16 <- t |> dplyr::filter(lags == 16)
  t12 <- t |> dplyr::filter(lags == 12)

  tf <- dplyr::full_join(t16, t12, by = c("defect", "measure", "buffer", "week")) |>
    dplyr::arrange(defect, measure, buffer, desc(week)) |>
    dplyr::mutate(buffer = paste0(buffer, "m buffer"),
                  o3.x = ifelse(is.na(o3.x), "", o3.x),
                  pm.x = ifelse(is.na(pm.x), "", pm.x),
                  o3.y = ifelse(is.na(o3.y), "", o3.y),
                  pm.y = ifelse(is.na(pm.y), "", pm.y),
                  measure = paste("Weekly", measure, "concentration"))

  b <- table(tf$measure)

  tf |> dplyr::select(-defect, -measure, -starts_with("lag")) |>
    kable(booktabs = TRUE, format = "latex", longtable = T, escape = F, #
          caption = my_caption, align = c("l", rep("c", 5)),
          col.names = c("Buffer", "Week", rep(c("O\\textsubscript{3}",
                                                "PM\\textsubscript{2.5}"), 2))) |>
    add_header_above(c(" " = 2, "16 week model" = 2, "12 week model" = 2)) |>
    kableExtra::collapse_rows(1, latex_hline = "major", valign = "top") |>
    kableExtra::group_rows(index = b, escape = F) |> #
    kableExtra::row_spec(0, bold=TRUE) |>
    kableExtra::row_spec(cumsum(b), hline_after = TRUE) |>
    # kableExtra::collapse_rows(1, latex_hline = "major", valign = "top") |>
    column_spec(1, bold = FALSE, width = "6em") |> #
    column_spec(2, bold = FALSE, width = "2em") |> #
    column_spec(3:6, bold = FALSE, width = "7em") |> #
    # column_spec(5:6, bold = FALSE, width = "8em") |> #
    kableExtra::kable_styling(
      latex_options = c("repeat_header"), # "hold_position", "striped",
      font_size = 10.5, # full_width = TRUE, position = "center",
      repeat_header_continued = "\\textit{(Continued on Next Page...)}") |>
    footnote(my_foot, escape = F)
}
