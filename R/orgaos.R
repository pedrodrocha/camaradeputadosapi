#' @title Get a list of bodies and commissions
#'
#' @param ... query parameters for the House of Representatives API (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info})
#'
#' @return A tibble of bodies and commissions
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos()
#' b <- orgaos(dataInicio = "2020-01-01", dataFim = "2020-12-31")
orgaos <- function(...) {
  query_list <- list(...)

  req <- main_api("orgaos",query_list)

  content <- req$dados

  if (length(content) == 0) {
    stop("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Extract an unique identifier for a given body or commission
#'
#' @param abbr A body or commission abbreviation
#'
#' @return A unique identifier for a given body or commission
#' @export
#' @family orgaos
#' @examples a <- orgaos_id(abbr = "PLEN")
orgaos_id <- function(abbr) {

  assertthat::assert_that(is.character(abbr))
  assertthat::assert_that(length(abbr) == 1)

  abbr <- stringr::str_to_upper(abbr)

  id <- orgaos(sigla = abbr)$id

  min(id)
}

#' @title Get information on a Brazilian House of Representatives body or commission
#'
#' @param id A body or commission unique identifier
#'
#' @return A tibble with information on a Brazilian House of Representatives body or commission
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_info(id = 180)
orgaos_info <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("orgaos/", id)
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop(e,call. = FALSE)
    }
  )

  content <- req$dados
  if (length(content) == 0) {
    stop("There is no data for this entry.", call. = FALSE)
  }


  content <- zero_or_null(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Get events that occurred or are on schedule for a given body or commission in between a time interval
#'
#' @param id A body or commission unique identifier
#' @param from The beginning date (YYYY-MM-DD) for the time interval
#' @param to The end date (YYYY-MM-DD) for the time interval
#' @param ... query parameters for the House of Representatives API (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info})
#'
#' @return A tibble with events that occurred or are on schedule for a given body or commission in between a time interval
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_eventos(id = 180, from = "2020-01-01", to = "2020-10-01")
orgaos_eventos <- function(id,from,to, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  assertthat::assert_that(!missing(from),msg = "'from' is missing")
  assertthat::assert_that(!missing(to),msg = "'to' is missing")

  from <- check_date(from)
  to <- check_date(to)

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(..., dataInicio = from, dataFim = to)

  path <- paste0("orgaos/", id, "/eventos")
  req <- main_api(path, query_list)

  content <- req$dados

  assertthat::assert_that(length(content) != 0, msg = "There is no data for this entry")

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get a list of positions of representatives on a body or commission
#'
#' @param id A body or commission unique identifier
#' @param ... query parameters for the House of Representatives API (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info})
#'
#' @return A tibble of positions of representatives on a body or commission
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_membros(id = 2007)
orgaos_membros <- function(id,...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  path <- paste0("orgaos/", id, "/membros")
  req <- main_api(path, query_list)

  content <- req$dados

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get information on voting procedures in a body or commission
#'
#' @param id A body or commission unique identifier
#' @param ... query parameters for the House of Representatives API (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info})
#'
#' @return A tibble with information on voting procedures in a body or commission
#' @export
#' @family orgaos
#' @examples a <- orgaos_votacoes(id = orgaos_id(abbr = "PLEN"))
orgaos_votacoes <- function(id, ...) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  path <- paste0("orgaos/", id, "/votacoes")
  req <- main_api(path, query_list)

  content <- req$dados

  `%!in%` <- Negate(`%in%`)
  tryCatch(
    assertthat::assert_that(length(content) > 0, msg = "There is no data for this entry"),
    error = function(e) {
      if ("dataInicio" %!in% names(query_list)) {
        stop(paste0("There is no data for this entry.", " Try setting a `dataInicio`"), call. = FALSE)

      } else {
        stop(e)
      }
    }
  )

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get metadata information on data related to 'orgaos'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'eventos'
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_referencias()
orgaos_referencias <- function(meta = c("idTipoOrgao","idSituacao")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("idTipoOrgao","idSituacao"),
      msg = "Unknow argument passed to 'meta'"
    )
  }

  path <- "referencias/orgaos"

  req <- main_api(path = path)

  content <- req$dados

  content$idTipoOrgao <- tibble::as_tibble(content$idTipoOrgao)
  content$idSituacao <- tibble::as_tibble(content$idSituacao)


  content[meta]
}
