#' @title Get a list of political parties that had or current have representatives in office
#'
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble with political parties that had or current have representatives in office
#' @export
#' @family partidos
#' @examples
#' a <- partidos()
#' b <- partidos(itens = 100, idLegislatura = 55)
partidos <- function(...){

  query_list <- list(...)

  req <- deputados_api("partidos",query_list)

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
  content <- suppressWarnings(partidos(sigla = abbr)$id)

  tryCatch(
    assertthat::assert_that(!is.null(content)),
    error = function(e) {
      stop("Not able to locate an id. Try again")
    }

  )

  content

}

#' @title Get information about a Brazilian political party
#'
#' @param id A Brazilian political party id
#'
#' @return A tibble with information about a Brazilian political party
#' @export
#' @family partidos
#' @examples
#' a <- partidos_info(partidos_id("PT"))
partidos_info <- function(id) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("partidos/", id)
  req <- deputados_api(path)

  content <- req$dados

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
#
#' @param id A Brazilian political party Id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble  of representatives in office for a given Brazilian political party
#' @export
#' @family partidos
#' @examples
#' a <- partidos_membros(id = 36844, itens = 100)
#' b <- partidos_membros(id = 36844, itens = 100, idLegislatura = 54)
partidos_membros <- function(id, ...) {

  query_list <- list(...)
  path <- paste0("partidos/",id, "/membros")

  req <- deputados_api(path,query_list)

  content <- req$dados

  if (length(tibble::as_tibble(content)) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get a list of blocs in the current legislative term at the Brazilian House of Representatives
#'
#' @return A tibble of blocs in the current legislative term at the Brazilian House of Representatives
#' @export
#' @family partidos
#' @examples
#' partidos_blocos()
partidos_blocos <- function() {

  req <- deputados_api("blocos")

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


