#' GET a list of legislative periods
#'
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return
#' @export
#' @family legislaturas
#' @examples
legislaturas <- function(...) {

  query_list <- list(...)

  req <- deputados_api("legislaturas",query_list)

  content <- req$dados

  tibble::as_tibble(content) %>%
    dplyr::select(-uri)

}
