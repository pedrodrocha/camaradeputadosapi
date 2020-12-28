#' @title Get a list of events that occurred or are on schedule at the House of Representatives
#'
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble  of events that occurred or are on schedule at the House of Representatives
#' @export
#' @family eventos
#' @examples
#' a <- eventos()
#' b <- eventos(dataInicio = "2020-01-01")
eventos <- function(...){

  query_list <- list(...)

  req <- main_api("eventos",query_list)

  content <- req$dados

  if (length(content) == 0) {
    warning("There is no data for this entry", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}


#' @title Get detailed information about a specific event
#'
#' @param id An event identifier
#'
#' @return A tibble with detailed information about a specific event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_info("59272")
eventos_info <- function(id) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id)
  req <- main_api(path)

  content <- req$dados

  content <- zero_or_null(content)

  localCamara <- zero_or_null(content$localCamara)
  content$localCamara <- NULL

  content <- tibble::as_tibble(content)

  tibble::as_tibble(localCamara) %>%
    dplyr::rename(
      "localNome" = nome,
      "localPredio" = predio,
      "localSala" = sala,
      "localAndar" = andar
    ) %>%
    dplyr::bind_cols(content,.) -> content

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uri" %in% names(content)) {
    content %>%
      dplyr::select(-c(uri, uriDeputados,uriConvidados))

  } else {
    content
  }

}

#' @title Get a list of representatives that were in a specific event
#'
#' @param id An event identifier
#'
#' @return A tibble of representatives that were in a specific event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_deputados(id = 60027)
eventos_deputados <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/deputados")
  req <- main_api(path)

  content <- req$dados

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri, uriPartido))

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Get a list of bodies, commissions, agencies, etc. that organized an event
#'
#' @param id An event identifier
#'
#' @return A tibble of bodies, commissions, agencies, etc. that organized an event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_orgaos(id = 60222)
eventos_orgaos <- function(id){

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/orgaos")
  req <- main_api(path)

  content <- req$dados

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Get a list of propositions that were or will be discussed in a given event
#'
#' @param id An event identifier
#'
#' @return A tibble of propositions that were or will be discussed in a given event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_pauta(id = 60027)
eventos_pauta <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/pauta")
  req <- main_api(path)

  content <- req$dados

  content <- zero_or_null(content)


  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uriVotacao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uriProposicaoRelacionada, uriVotacao))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information for a voting event
#'
#' @param id An event identifier
#'
#' @return A tibble with information for a voting event
#' @export
#' @family eventos
#'
#' @examples
#' a <- eventos_votacoes(id = 60241)
eventos_votacoes <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/votacoes")
  req <- main_api(path)

  content <- req$dados

  content <- zero_or_null(content)

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}
