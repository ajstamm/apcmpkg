# possibly also use for aim 2 cum table and a variant for aim 1 table(s)
# obsolete?


bd_rr2_tables <- function(my_sheet, my_caption, bold_rows = 0) {
  my_foot <- paste("Model was adjusted for maternal education level,",
                   "maternal smoking, tract-level median income,",
                   "conception season, and the indicated green space ")
  f2 <- paste("variable. Week 0 is week of conception.",
              "Week 12 is the end of the first trimester of pregnancy.")
  # if (grepl("cf", my_sheet)) {
  #   f2 <- c(paste(f2, "For clubfoot, only the 16-week model was run because limb formation"),
  #       "finishes around week 12.")
  # }
  my_foot <- c(my_foot, f2)

  t <- readxl::read_xls("data/model2rr_tables.xls", sheet = my_sheet) |>
    dplyr::filter(!is.na(bd)) |>
    dplyr::mutate(greenlbl = ifelse(grepl("green", green),
                                    "composite of grass, trees, and water within buffer",
                                    "composite of grass and trees within buffer"),
                  pollutant = paste("Weekly", air, measure, "concentration,", greenlbl),
                  week = 12 - lag) |> dplyr::select(-lag) |>
    dplyr::arrange(pollutant, week)


  n <- c("Week", paste0(c(50, seq(100, 500, by = 100)), "m buffer"))
  r <- table(t$pollutant)

  t |> dplyr::select(week, b_50:b_500) |>
    kable(booktabs = TRUE, format = "latex", longtable = T, escape = F, #
          caption = my_caption, col.names = n, align = rep("c", 7)) |>
    row_spec(bold_rows, bold=TRUE) |>
    column_spec(1, bold = TRUE, width = "2.5em") |> #
    column_spec(2:7, bold = FALSE, width = "8em") |> #
    kableExtra::group_rows(index = r, escape = F) |> #
    kableExtra::row_spec(cumsum(r), hline_after = TRUE) |>
    # column_spec(5:6, bold = FALSE, width = "8em") |> #
    kableExtra::kable_styling(
      latex_options = c("repeat_header"), # "hold_position", "striped",
      font_size = 10.5, # full_width = TRUE, position = "center",
      repeat_header_continued = "\\textit{(Continued on Next Page...)}") |>
    footnote(my_foot)
}
