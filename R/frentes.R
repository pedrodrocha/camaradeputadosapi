#' @title Get a list of parliamentary fronts
#'
#' @description
#'
#' Get a list of parliamentary fronts from the Brazilian House of representatives API that were created after 2003.
#' A parliamentary front exist until the end of the legislative period in which it was created.
#'
#' @param idLegislatura An unique identifier for one or more legislative periods. You can search for those with \code{\link{legislaturas_id}}
#'
#' @return A tibble of parliamentary fronts
#' @export
#' @family frentes
#'
#' @examples
#' a <- frentes(idLegislatura = 56)
frentes <- function(idLegislatura) {

  assertthat::assert_that(!missing(idLegislatura),msg = "'idLegislatura' is missing")
  idLegislatura <- paste0(idLegislatura,collapse = ",")

  query_list <- list(idLegislatura = idLegislatura)

  req <- main_api("frentes",query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Extract a parliamentary front Id
#'
#' @param nome A name or part of a name from a parliamentary front.
#' @param idLegislatura An unique identifier for a legislative period. You can search for it using \code{\link{legislaturas_id}}
#'
#' @return A parliamentary front id
#' @export
#' @family frentes
#' @examples
#' a <- frentes_id(nome = "fortalecimento do sus", idLegislatura = 56)
frentes_id <- function(nome, idLegislatura){
  assertthat::assert_that(!missing(idLegislatura),msg = "'idLegislatura' is missing")
  assertthat::assert_that(!missing(nome),msg = "'nome' is missing")

  nome <- stringr::str_to_lower(nome)

  content <- frentes(idLegislatura) %>%
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
#' @description
#' Get general information about a parliamentary front identified by an unique id
#'
#' @param id An unique identifier for a parliamentary front
#'
#' @return A tibble with information about a parliamentary front
#' @export
#' @family frentes
#'
#' @examples
#' a <- frentes_info(id = 54257)
frentes_info <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }
  path <- paste0("frentes/",id)
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find a parliamentary front for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados
  not_zero_content(content)

  content <- zero_or_null(content)


  coordenador <- zero_or_null(content$coordenador)


  content$coordenador <- NULL
  content <- tibble::as_tibble(content)

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
#' @description
#' Get a list of members for a given parliamentary front and what position each member hold.
#'
#' @param id An unique identifier for a parliamentary front
#'
#' @return A tibble of members for a given parliamentary front
#' @export
#' @family frentes
#' @examples
#' a <- frentes_membros(id = 54257)
frentes_membros <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("frentes/",id, "/membros")
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find a parliamentary front for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados
  not_zero_content(content)


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri, uriPartido))

  } else {
    tibble::as_tibble(content)
  }


}


