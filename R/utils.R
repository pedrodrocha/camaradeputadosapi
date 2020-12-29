#' @title Check a list for zero or null entries
#'
#' @param list receives a list
#'
#' @return a list that has been cleared of length 0 and NULL values
zero_or_null <- function (list){

  zero_null <- rlist::list.which(list, length(.) < 1)

  if (length(zero_null) > 0) {
    for (i in seq_along(zero_null)) {
      list[[zero_null[i]]] <- NA
    }
  }

  return(list)

}


