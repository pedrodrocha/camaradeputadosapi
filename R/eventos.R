#' @title Get a list of events that occurred or are on schedule at the House of Representatives
#'
#' @description
#' Get a list of events that occurred or are on schedule at the Brazilian House of Representatives.
#' If query parameters from the API such as 'dataInicio' or 'dataFim' are not used, it will return events that took place or are scheduled to occur five days before and after the query.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#' @return A tibble of events that occurred or are on schedule at the Brazilian House of Representatives
#' @export
#' @family eventos
#' @examples
#' a <- eventos(dataInicio = "2020-12-01", dataFim = "2020-12-01")
eventos <- function(...){

  query_list <- list(...)

  check_api_parameters(names(query_list),query_list)

  req <- main_api("eventos",query_list)

  content <- req$dados
  not_zero_content(content)



  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}


#' @title Get information about a specific event
#'
#' @description
#' Get detailed information on a event from the Brazilian House of Representatives.
#' You can look for events running \code{\link{eventos}}.
#'
#' @param id An unique identifier for an event
#'
#' @return A tibble with detailed information about a specific event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_info(id = "59272")
eventos_info <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id)
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find an event for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados

  not_zero_content(content)

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


  if ("uri" %in% names(content)) {
    content %>%
      dplyr::select(-c(uri, uriDeputados,uriConvidados))

  } else {
    content
  }

}

#' @title Get a list of representatives that attended a specific event
#'
#' @description
#'
#' Get a list of representatives that attended a specific event. You can look for events running \code{\link{eventos}}.
#' If the event already took place the list identifies representatives that actually attended.
#' If the event is on schedule the list identifies representatives that have the event on their agenda.
#'
#' @param id An unique identifier for an event
#'
#' @return A tibble of representatives that attended a specific event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_deputados(id = 60027)
eventos_deputados <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/deputados")
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find an event for the id '",id,"'", call. = FALSE)
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

#' @title Get a list of bodies, commissions, agencies, etc. that organized an event
#'
#' @description
#'  Get a list of bodies, commissions, agencies, etc. that organized an event.
#'  You can look for events running \code{\link{eventos}}.
#'
#' @param id An unique identifier for an event
#'
#' @return A tibble of bodies, commissions, agencies, etc. that organized an event
#' @export
#' @family eventos
#' @examples
#' a <- eventos_orgaos(id = 60222)
eventos_orgaos <- function(id){

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/orgaos")
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find an event for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados
  not_zero_content(content)


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Get a list of propositions that were or will be discussed in an event
#'
#' @description
#' If the event is a deliberative event, get a list of propositions that were discussed or are scheduled to be discussed at that event.
#' You can look for events running \code{\link{eventos}}.
#'
#' @param id An unique identifier for an event
#'
#' @return A tibble of propositions
#' @export
#' @family eventos
#' @examples
#' a <- eventos_pauta(id = 60027)
eventos_pauta <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/pauta")
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find an event for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados

  content <- zero_or_null(content)
  not_zero_content(content)

  if ("uriVotacao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uriProposicaoRelacionada, uriVotacao))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information for voting processes in an event
#'
#' @description
#'
#' Get basic information on votings that occurred in an event. Note that votings only occur on deliberative events.
#' You can look for events running \code{\link{eventos}}.
#' More information about voting processes can be obtained with \code{\link{votacoes_info}}.
#'
#' @param id An unique identifier for an event
#'
#' @return A tibble with information for voting processes in an event
#' @export
#' @family eventos
#'
#' @examples
#' a <- eventos_votacoes(id = 60241)
eventos_votacoes <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("eventos/", id, "/votacoes")
  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 not found. Couldn't find an event for the id '",id,"'", call. = FALSE)
    }
  )

  content <- req$dados

  content <- zero_or_null(content)
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get metadata information on data related to 'eventos'
#'
#' @description
#'
#' Get metadata information of data related to 'eventos'. Two types of metadata are available: a) 'codTipoEvento', with a list of event types; b) 'siglaSituacao', with a list of status for events.
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'eventos'
#' @export
#' @family eventos
#' @examples
#' a <- eventos_referencias()
eventos_referencias <- function(meta = c("codTipoEvento","codSituacao")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("codTipoEvento","codSituacao"),
      msg = "Unknow argument passed to 'meta'"
    )
  }

  path <- "referencias/eventos"

  req <- main_api(path = path)

  content <- req$dados
  not_zero_content(content)

  content$codTipoEvento <- tibble::as_tibble(content$codTipoEvento)
  content$codSituacao <- tibble::as_tibble(content$codSituacao)


  content[meta]
}
