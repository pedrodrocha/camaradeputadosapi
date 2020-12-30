#' @title Get a list of bodies and commissions
#'
#' @description
#' Get a list of bodies and commissions from the Brazilian House of Representatives
#' You can use further query parameters for filter the list.
#' For example for filter according to the type you can use 'codTipoOrgao'.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param ... Further query parameters from the House of Representatives API
#'
#' @return A tibble of bodies and commissions from the Brazilian House of Representatives
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos()
#' \donttest{
#'  b <- orgaos(codTIpoOrgao = 1)
#' }
orgaos <- function(...) {
  query_list <- list(...)
  check_api_parameters(names(query_list),query_list)
  req <- main_api("orgaos",query_list)

  content <- req$dados
  not_zero_content(content)

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
  assertthat::assert_that(!missing(abbr),msg = "'abbr' is missing")
  assertthat::assert_that(is.character(abbr))
  assertthat::assert_that(length(abbr) == 1)

  abbr <- stringr::str_to_upper(abbr)

  id <- orgaos(sigla = abbr)$id

  min(id)
}

#' @title Get information on a Brazilian House of Representatives body or commission
#'
#' @description
#' Get all the information available on a given Brazilian House of Representatives body or commission
#'
#' @param id A body or commission unique identifier
#'
#' @return A tibble with information on a Brazilian House of Representatives body or commission
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_info(id = 180)
orgaos_info <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("orgaos/", id)
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop(paste0("404 Not Found. Couldn't find a body or commission for id'",id,"'"), call. = FALSE)
    }
  )

  content <- req$dados
  not_zero_content(content)


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
#' @description
#' Get events that occurred or are on schedule for a given body or commission in between a time interval.
#' The maximum limit of results is 100 per query.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).

#'
#' @param id A body or commission unique identifier
#' @param from The beginning date (YYYY-MM-DD) for the time interval
#' @param to The end date (YYYY-MM-DD) for the time interval
#' @param ... Further query parameters from the House of Representatives API
#'
#' @return A tibble with events that occurred or are on schedule for a given body or commission in between a time interval
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_eventos(id = 180, from = "2020-01-01", to = "2020-03-01")
orgaos_eventos <- function(id,from,to, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  assertthat::assert_that(!missing(from),msg = "'from' is missing")
  assertthat::assert_that(!missing(to),msg = "'to' is missing")

  from <- check_date(from)
  to <- check_date(to)

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(..., dataInicio = from, dataFim = to, itens = 100)
  check_api_parameters(names(query_list),query_list)
  max_limit100(names(query_list),query_list)

  path <- paste0("orgaos/", id, "/eventos")
  req <- main_api(path, query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get a list of roles of representatives members of a body or commission
#'
#' @description
#'
#' Get information on roles of representatives on a body or commission at the Brazilian House of Representatives.
#' If query parameters from the API such as 'dataInicio' or 'dataFim' are not used, it will return members and their roles at the time of the function call.
#'
#' @param id A body or commission unique identifier
#' @param ... Further query parameters from the House of Representatives API
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
  check_api_parameters(names(query_list),query_list)
  path <- paste0("orgaos/", id, "/membros")
  req <- main_api(path, query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get information on votings in a body or commission
#'
#' @description
#' Get information on votings occurred in a given body or commission from the Brazilian House of Representatives.
#' If the body or commission is permanent and query parameters from the API such as 'dataIncio' and 'dataFim' are not used, it will return votings from the last 30 days.
#' If the body or commission is not permanent, it will return all votings.
#' By default it returns the maximum query limit of 200 results.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param id A body or commission unique identifier
#' @param ... Further query parameters from the House of Representatives API
#' @return A tibble with information on voting procedures in a body or commission
#' @export
#' @family orgaos
#' @examples
#' a <- orgaos_votacoes(id = 180, dataInicio = "2020-12-01", dataFim = "2020-12-02")
orgaos_votacoes <- function(id, ...) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(..., itens = 200)
  check_api_parameters(names(query_list),query_list)

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


#' @title Get metadata information for data related to 'orgaos'
#'
#'
#' @description
#'
#' Get metadata information for data related to 'orgaos'.
#' Two types of metadata are available:
#' a)'idTipoOrgao': Has information on the types of bodies and commissions at the Brazilian House of Representatives;
#' b)'idSituacao': Has information on the status of bodies and commissions at the Brazilian House of Representatives.
#'
#' @param meta Types of metadata
#'
#' @return A list of tibbles with metadata information on data related to the family 'orgaos'
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
