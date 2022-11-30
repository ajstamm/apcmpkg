#' Identify week number
#'
#' @param days      Vector of days in the format "2011-01-01", as either a
#'                  string or a date class.
#' @param start_day Day the week should begin. Defaults to Sunday.
#'
#'
#' This function identifies which week each day falls into.
#' It can be used to calculate weekly measures.
#'
list_weeks <- function(days, start_day = "Sunday") {
  if (!class(days) == "Date") days <- as.Date(days)
  days <- unique(days[order(days)])
  day_date <- paste(days, weekdays(days, abbreviate = TRUE))
  weeks <- list()
  temp <- c()
  j = 1
  for (i in day_date) {
    temp <- c(temp, i)
    if (grepl(substr(start_day, 1, 3), i)) {
      j <- j + 1
      temp <- i
    }
    weeks[[j]] <- temp
  }
  weeks[sapply(weeks, is.null)] <- NULL
  names(weeks) <- paste0("w", formatC(1:length(weeks), format = "d", flag = 0,
                                  width = nchar(length(weeks))))
  w <- data.frame(week = names(unlist(weeks)), day = unlist(weeks)) |>
    dplyr::mutate(week = substr(week, 1, nchar(length(weeks)) + 1),
                  date = as.Date(substr(day, 1, 10)))

  return(w)
}
