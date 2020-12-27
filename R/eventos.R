#' @title Get a list of events that ocurred or are on schedule at the House of Representatives
#'
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble  of events that ocurred or are on schedule at the House of Representatives
#' @export
#' @family eventos
#' @examples
#' a <- eventos()
#' b <- eventos(dataInicio = "2020-01-01")
eventos <- function(...){

  query_list <- list(...)

  req <- deputados_api("eventos",query_list)

  content <- req$dados

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }


}
