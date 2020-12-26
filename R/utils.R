#' Title
#'
#' @param list receives a list
#'
is.zero_or_null <- function (list){
  rlist::list.which(list, length(.) < 1)
}
