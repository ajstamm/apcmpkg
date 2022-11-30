#' Calculate weekly peaks and means
#' 
#' @param d         Dataset.
#' @param id_var    Unique ID for areas.
#' @param date_var  Date variable as a date or as a string in the format 
#'                  "2000-01-01".
#' @param value_var The value to summarize. At present, values must be handled 
#'                  one at a time.
#' 
#' 
#' 

weekly_stats <- function(d, id_var, date_var, value_var, 
                         stats = c("min", "mean", "max")) {
  n <- d |> dplyr::rename(id = !!dplyr::sym(id_var), 
                          date = !!dplyr::sym(date_var), 
                          value = !!dplyr::sym(value_var)) |>
    dplyr::select(id, date, value) 
  if (!"Date" %in% class(n$date)) n <- dplyr::mutate(n, date = as.Date(date))

  w <- list_weeks(days = n$date, start_day = "Tuesday") |>
    dplyr::full_join(n, by = "date") |> 
    dplyr::group_by(id, week)
  l <- list()
  if ("mean" %in% stats) l$mean <- w |> dplyr::summarize(mean = mean(value, na.rm = TRUE))
  if ("min" %in% stats) l$min <- w |> dplyr::summarize(min = min(value, na.rm = TRUE))
  if ("max" %in% stats) l$max <- w |> dplyr::summarize(max = max(value, na.rm = TRUE))
  f <- purrr::reduce(l, dplyr::full_join, by = c("id", "week"))
  q <- dplyr::full_join(w, f, by = c("id", "week")) |> 
    dplyr::rename(!!dplyr::sym(id_var) := "id", 
                  !!dplyr::sym(date_var) := "date", 
                  !!dplyr::sym(value_var) := "value")
  
  return(q)
}





