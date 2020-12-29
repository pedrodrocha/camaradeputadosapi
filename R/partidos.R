#' @title Get a list of political parties
#'
#' @description
#' Get a list of political parties that had or currently have representatives at the Brazilian House of Representatives.
#' If query parameters from the API such as 'dataInicio', 'dataFim' and 'idLegislatura' are not used, it will return only parties that currently have a representative.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#'
#' @param ... Further query parameters from the House of Representatives API
#'
#' @return A tibble with political parties
#' @export
#' @family partidos
#' @examples
#' a <- partidos(idLegislatura = "56")
partidos <- function(...){



  query_list <- list(..., itens = 100)

  if ("sigla" %in% names(query_list)) {
    query_list$sigla <- paste0(query_list$sigla, collapse = ",")
  }

  check_api_parameters(names(query_list),query_list)

  req <- main_api("partidos",query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Extract a Brazilian Political Party ID
#'
#' @param abbr A Brazilian political party abbreviation
#'
#' @return An id for a given political party
#' @export
#' @family partidos
#'
#' @examples
#' a <- partidos_id(abbr = "PT")
partidos_id <- function(abbr) {
  assertthat::assert_that(!missing(abbr),msg = "'abbr' is missing")
  content <- suppressWarnings(partidos(sigla = abbr)$id)
  not_zero_content(content)

  content

}

#' @title Get information about a Brazilian political party
#'
#' @description
#'
#' Get information about a given Brazilian political party
#'
#' @param id A Brazilian political party unique identifier
#'
#' @return A tibble with information about a Brazilian political party
#' @export
#' @family partidos
#' @examples
#' a <- partidos_info(id = 36844)
partidos_info <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("partidos/", id)
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  content <- zero_or_null(content)

  status <- zero_or_null(content$status)
  content$status <- NULL

  lider <- zero_or_null(status$lider)
  status$lider <- NULL

  status <- tibble::as_tibble(status)
  lider <- tibble::as_tibble(lider) %>%
    dplyr::select(-c(uri,uriPartido, siglaPartido)) %>%
    dplyr::rename(
      "lider_nome" = nome,
      "lider_uf" = uf,
      "lider_urlFoto" = urlFoto,
      "lider_idLegislatura" = idLegislatura
    )


  content <- tibble::as_tibble(content)

  content %>%
    dplyr::bind_cols(.,status) %>%
    dplyr::bind_cols(.,lider) -> content

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    content %>%
      dplyr::select(-c(uri,uriMembros))

  } else {
    content
  }
}

#' @title Get a list of representatives that are or were in office for a given Brazilian political party
#'
#' @description
#'
#' Get a list of representatives that are or were in office for a given political party during a legislative period.
#' If query parameters from the API such as 'dataInicio', 'dataFim' and 'idLegislatura' are not used, it will return representatives for the currently legislative period.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param id A Brazilian political party unique identifier
#' @param ... Further query parameters from the Brazilian House of Representatives API
#'
#' @return A tibble  of representatives in office for a given Brazilian political party
#' @export
#' @family partidos
#' @examples
#' a <- partidos_membros(id = 36844)
partidos_membros <- function(id, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...,itens = 100)
  check_api_parameters(names(query_list), query_list)

  path <- paste0("partidos/",id, "/membros")

  req <- main_api(path,query_list)

  content <- req$dados
  not_zero_content(content)


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get a list of blocs at the Brazilian House of Representatives
#'
#' @description
#'
#' Get a list of blocs of political parties in the current legislative period at the Brazilian House of Representatives
#'
#' @return A tibble of blocs in the current legislative term at the Brazilian House of Representatives
#' @export
#' @family partidos
#' @examples
#' a <- partidos_blocos()
partidos_blocos <- function() {

  req <- main_api("blocos")

  content <- req$dados

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri) %>%
      dplyr::rename("composicao" = nome)

  } else {
    tibble::as_tibble(content) %>%
      dplyr::rename("composicao" = nome)
  }
}


