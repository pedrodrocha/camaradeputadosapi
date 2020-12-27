#' GET a list of legislative periods
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
legislaturas <- function(...) {

  query_list <- list(...)

  req <- deputados_api("legislaturas",query_list)

  content <- req$dados

  tibble::as_tibble(content) %>%
    dplyr::select(-uri)

}
