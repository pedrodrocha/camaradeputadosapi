#' @title Check a list for zero or null entries
#'
#' @param list receives a list
#'
zero_or_null <- function (list){

  zero_null <- rlist::list.which(list, length(.) < 1)

  if (length(zero_null) > 0) {
    for (i in seq_along(zero_null)) {
      list[[zero_null[i]]] <- NA
    }
  }

  return(list)

}


#' @title Check date
#'
#' @param date a date entry for check
#'
#' @return a date
check_date <- function(date) {
  date <- suppressWarnings(lubridate::ymd(date))

  tryCatch(
    assertthat::assert_that(!is.na(date)),
    error = function(e) {
      stop("Wrong date format. Try 'YYYY-MM-DD'")
    }
  )

  if(!is.character(date)){
    date <- as.character(date)
  }

  date
}
