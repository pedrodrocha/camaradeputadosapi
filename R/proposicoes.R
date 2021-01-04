#' @title Get a list of propositions presented at the Brazilian House of Representatives API
#'
#' @description
#' Get a list of propositions presented at the Brazilian House of Representatives API.
#' By default it returns the max query limit of 100 propositions active on the last 30 days.
#' You can control for proceeding periods using the API parameters 'dataInicio' and 'dataFim' and for the date the proposition was first presented using 'dataApresentacaoInicio' and 'dataApresentacaoFim'.
#' You can also filter propositions by thematic area with 'codTema'.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param ... Further query parameters from the House of Representatives API
#'
#' @return A tibble of propositions discussed in between a time interval
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes(dataInicio = "2020-12-01", dataFim = "2020-12-01")
proposicoes <- function(...) {
  query_list <- list(
    itens = 100,
    ...
  )
  check_api_parameters(names(query_list),query_list)

  req <- main_api("proposicoes",query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title  Extract a proposition unique identifier
#'
#' @description
#'
#' Extract a proposition unique identifier from its standard name (Ex: "PL 1665/2020")
#'
#'
#' @param type The type of proposition. Ex: 'PL' stands for 'Projeto de Lei' or Draft Bill.
#' @param number The number of the proposition. Ex: 1665
#' @param year The year of the proposition. Ex: 2020
#'
#' @return A proposition uniquer identifier
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_id(type = "PL",number = 1665, year = 2020)

proposicoes_id <- function(type, number, year) {
  assertthat::assert_that(!missing(type),msg = "'type' is missing")
  assertthat::assert_that(!missing(number),msg = "'number' is missing")
  assertthat::assert_that(!missing(year),msg = "'year' is missing")

  content <- proposicoes(siglaTipo = type, numero = number, ano = year)

  content$id

}


#' @title Get information about a proposition of Brazilian House of Representatives
#'
#' @description
#'
#' Get detailed information about a proposition presented at the Brazilian House of representatives
#' @param id A proposition unique identifier
#'
#' @return A tibble with information about a proposition
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_info(id = 19175)
proposicoes_info <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id)
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  content <- zero_or_null(content)
  content <- tibble::as_tibble(content)
  content$statusProposicao <- tibble::as_tibble(zero_or_null(content$statusProposicao))


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgaoNumerador,uriAutores,uriPropPrincipal,uriPropAnterior,uriPropPosterior)) %>%
      dplyr::distinct()

  } else {
    tibble::as_tibble(content) %>%
      dplyr::distinct()
  }


}

#' @title  Get a list of authors for a given proposition
#'
#' @description
#'
#' Get a list of authors for a given proposition. Note: not only representatives are authors.
#' By the House of Representatives regiment every signatory is considered author.
#'
#' @param id A proposition unique identifier
#'
#' @return A tibble with authors of a given proposition
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_autores(id = 19175)
proposicoes_autores <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/autores")
  req <- main_api(path)

  content <- req$dados

  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get a list of propositions related to a given proposition
#'
#' @description
#' Get a list of propositions from any type that are related to a given proposition
#'
#' @param id A proposition unique identifier
#'
#' @return A tibble of propositions related to a given proposition
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_relacionadas(id = 2244218)
proposicoes_relacionadas <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/relacionadas")
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get a list of thematic areas related to a given proposition
#'
#' @description
#'
#' Get a list of thematic areas for a given proposition, according to classification from the House of Representatives Center of Information and Documentation.
#'
#'
#' @param id A proposition unique identifier
#'
#' @return A tibble of thematic areas related to a given proposition
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_temas(id = 15990)
proposicoes_temas <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/temas")
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get the historical record of a proposition at the House of Representatives
#'
#' @description
#'
#' Get the historical record of a proposition at the House of Representatives since it was first presented.
#' @param id A proposition unique identifier
#'
#' @return A tibble with the historical record of a proposition at the House of Representatives
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_historico(id = 19175)
proposicoes_historico <- function(id){
  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/tramitacoes")
  req <- main_api(path)

  content <- req$dados

  not_zero_content(content)

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uriOrgao,uriUltimoRelator))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information on voting procedures for a proposition and for those that affected a proposition
#'
#' @description
#' Get basic information on voting procedures for a proposition and for those that affected a proposition.
#'
#' @param id A proposition unique identifier
#'
#' @return A tibble with information on voting procedures of a proposition and that affected a proposition
#' @export
#' @family proposicoes
#' @examples
#' a <- proposicoes_votacoes(id = 2244218)
proposicoes_votacoes <- function(id){
  assertthat::assert_that(!missing(id),msg = "'type' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("proposicoes/",id, "/votacoes")
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriEvento,uriOrgao,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get metadata information for data related to 'proposicoes'
#'
#'
#' @description
#'
#' Get metadata information for data related to 'proposicoes'.
#' Five types of metadata are available: a) 'siglaTipo', with types of propositions;
#' b) 'codSituacao', with types of status for a proposition;
#' c) 'tiposTramitacao', with types of procedures for a proposition;
#' d) 'codTema', with types of thematic areas a proposition can be classified;
#' e) 'codTipoAuto', with actors that can be authors of a proposition
#'
#' @param meta Types of metadata
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
  not_zero_content(content)

  content$siglaTipo <- tibble::as_tibble(content$siglaTipo)
  content$codSituacao <- tibble::as_tibble(content$codSituacao)
  content$tiposTramitacao <- tibble::as_tibble(content$tiposTramitacao)
  content$codTema <- tibble::as_tibble(content$codTema)
  content$codTipoAutor <- tibble::as_tibble(content$codTipoAutor)

  content[meta]
}
