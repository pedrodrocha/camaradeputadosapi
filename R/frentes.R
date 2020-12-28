#' @title Get a list of parliamentary fronts
#'
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble of parliamentary fronts
#' @export
#' @family frentes
#'
#' @examples
#' a <- frentes()
#' b <- frentes(idLegislatura = 54)
frentes <- function(...) {
  query_list <- list(...)

  req <- deputados_api("frentes",query_list)

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

#' @title Extract a parliamentary front Id
#'
#' @param nome A name or part of a name from a parliamentary front
#'
#' @return A parliamentary front id
#' @export
#' @family frentes
#' @examples
#' a <- frentes_id("fortalecimento do sus")
frentes_id <- function(nome){

  nome <- stringr::str_to_lower(nome)

  content <- frentes() %>%
    dplyr::mutate(titulo = stringr::str_to_lower(titulo))

  content %>%
    dplyr::filter(stringr::str_detect(titulo, {{nome}} )) %>%
    dplyr::mutate(titulo = stringr::str_to_title(titulo)) -> content

  content <- content$id

  if (length(content) > 1) {

    stop("More than one match, can you be more specific ?")

  } else if (length(content) == 0) {
    stop("Couldn't find any matches. Try again")

  }

  content

}

#' @title Get general information about a parliamentary front
#'
#' @param id Parliamentary front identifier
#'
#' @return A tibble with information about a parliamentary front
#' @export
#' @family frentes
#'
#' @examples
#' a <- frentes_info(id = frentes_id("fortalecimento do sus"))
frentes_info <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("frentes/",id)
  req <- deputados_api(path)

  content <- req$dados

  content <- zero_or_null(content)


  coordenador <- zero_or_null(content$coordenador)


  content$coordenador <- NULL
  content <- tibble::as_tibble(content)

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)

  }

  tibble::as_tibble(coordenador) %>%
    dplyr::select(-c(uri, uriPartido)) %>%
    dplyr::rename(
      "coord_id" = id,
      "coord_nome" = nome,
      "coord_siglaUf" = siglaUf,
      "coord_idLegislatura"= idLegislatura,
      "coord_urlFoto" = urlFoto,
      "coord_email" = email
    ) %>%
    dplyr::bind_cols(content,.) -> content

  if ("uri" %in% names(content)) {
    content %>%
      dplyr::select(-uri)
  } else {
    content
  }
}

#' @title Get a list of members for a given parliamentary front
#'
#' @param id A parliamentary front identifier
#'
#' @return A tibble of members for a given parliamentary front
#' @export
#' @family frentes
#' @examples
#' a <- frentes_membros(frentes_id("fortalecimento do sus"))
frentes_membros <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("frentes/",id, "/membros")
  req <- deputados_api(path)

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


