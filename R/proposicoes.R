#' @title Get a list of propositions discussed in between a time interval
#'
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#' @param from start date (YYYY-MM-DD) for the time interval in which a proposition was discussed
#' @param to end date (YYYY-MM) for the time interval in which a proposition was discussed
#'
#' @return A tibble of propositions discussed in between a time interval
#' @export
#' @family proposicoes
#' @examples proposicoes(from = "2020-01-01", to = "2020-12-01", itens = 100)
proposicoes <- function(..., from, to) {

  assertthat::`%has_args%`(proposicoes,args = c(from,to))
  from <- check_date(from)
  to <- check_date(to)


  query_list <- list(
    dataInicio = from,
    dataFim = to,
    ...
  )

  req <- main_api("proposicoes",query_list)

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

#' @title Get detailed information about a proposition of Brazilian House of Representatives
#'
#' @param id A proposition identifier
#'
#' @return A tibble with information about a proposition
#' @export
#' @family proposicoes
#' @examples proposicoes_info(id = 15990)
proposicoes_info <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id)
  req <- main_api(path)

  content <- req$dados

  content <- zero_or_null(content)
  content <- tibble::as_tibble(content)
  content$statusProposicao <- tibble::as_tibble(zero_or_null(content$statusProposicao))

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgaoNumerador,uriAutores,uriPropPrincipal,uriPropAnterior,uriPropPosterior)) %>%
      dplyr::distinct()

  } else {
    tibble::as_tibble(content) %>%
      dplyr::distinct()
  }


}

#' @title  Get a list with authors of a given proposition
#'
#' @param id A proposition identifier
#'
#' @return A tibble with authors of a given proposition
#' @export
#' @family proposicoes
#' @examples proposicoes_autores(id = 15990)
proposicoes_autores <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/autores")
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

#' @title Get a list of propositions related to a given proposition
#'
#' @param id A proposition identifier
#'
#' @return A tibble of propositions related to a given proposition
#' @export
#' @family proposicoes
#' @examples proposicoes_relacionadas(id = 15990)
proposicoes_relacionadas <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/relacionadas")
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


#' @title Get a list of thematic areas related to a given proposition
#'
#' @param id A proposition identifier
#'
#' @return A tibble of thematic areas related to a given proposition
#' @export
#' @family proposicoes
#' @examples proposicoes_temas(id = 15990)
proposicoes_temas <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/temas")
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

#' @title Get the historical record of a proposition at the House of Representatives
#'
#' @param id A proposition identifier
#'
#' @return A tibble with the historical record of a proposition at the House of Representatives
#' @export
#' @family proposicoes
#' @examples proposicoes_historico(id = 15990)
proposicoes_historico <- function(id){
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/tramitacoes")
  req <- main_api(path)

  content <- req$dados

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uriOrgao,uriUltimoRelator))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information on voting procedures of a proposition and that affected a proposition
#'
#' @param id A proposition identifier
#'
#' @return A tibble with information on voting procedures of a proposition and that affected a proposition
#' @export
#' @family proposicoes
#' @examples proposicoes_votacoes(id = 17823)
proposicoes_votacoes <- function(id){
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/votacoes")
  req <- main_api(path)

  content <- req$dados

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriEvento,uriOrgao,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get metadata information on data related to 'proposicoes'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to 'proposicoes'
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_referencias()
proposicoes_referencias <- function(meta = c("siglaTipo","codSituacao","tiposTramitacao","codTema","codTipoAutor")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("siglaTipo","codSituacao","tiposTramitacao","codTema","codTipoAutor"),
      msg = "Unknow argument passed to 'meta'"
    )
  }


  path <- "referencias/proposicoes"

  req <- main_api(path = path)

  content <- req$dados

  content$siglaTipo <- tibble::as_tibble(content$siglaTipo)
  content$codSituacao <- tibble::as_tibble(content$codSituacao)
  content$tiposTramitacao <- tibble::as_tibble(content$tiposTramitacao)
  content$codTema <- tibble::as_tibble(content$codTema)
  content$codTipoAutor <- tibble::as_tibble(content$codTipoAutor)

  content[meta]
}
