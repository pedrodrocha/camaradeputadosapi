#' @title Get a list of voting processes
#'
#' @description
#'
#' Get a list of voting processes on several bodies from the House of Representatives
#' By default it returns the maximum query limit of 200 results.
#' If query parameters from the API such as 'dataInicio', and 'dataFim'  are not used, it will return only results from the last 30 days.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param ... Further query parameters from the House of Representatives API
#'
#' @return A tibble of voting processes
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes(dataInicio = "2020-12-01",dataFim = "2020-12-02")
votacoes <- function(...) {

  query_list <- list(..., itens = 200)
  check_api_parameters(names(query_list),query_list)
  max_limit200(names(query_list),query_list)

  req <- main_api("votacoes",query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information on voting processes
#'
#' @description
#'
#' Get detailed information on a given voting process occurred at the Brazilian House of Representatives.
#'
#' @param id An unique identifier for a voting process. You can find ids running \code{\link{votacoes}}
#'
#' @return A list of tibbles with detailed information on a voting process
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_info(id = "2265603-43")
votacoes_info <- function(id) {
  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("votacoes/",id)

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop(paste0("404 Not Found. Couldn't find a voting process for id'",id,"'"), call. = FALSE)
    }
  )

  content <- req$dados
  not_zero_content(content)

  content <- zero_or_null(content)

  ultimaApresentacaoProposicao <- tibble::as_tibble(zero_or_null(content$ultimaApresentacaoProposicao))
  objetosPossiveis <- tibble::as_tibble(content$objetosPossiveis)
  proposicoesAfetadas <- tibble::as_tibble(content$proposicoesAfetadas)

  content$objetosPossiveis <- NULL
  content$proposicoesAfetadas <- NULL
  content$ultimaApresentacaoProposicao <- NULL

  content <- tibble::as_tibble(content)

  if ("uriProposicaoCitada" %in% names(ultimaApresentacaoProposicao)) {
    ultimaApresentacaoProposicao <- ultimaApresentacaoProposicao %>%
      dplyr::select(-uriProposicaoCitada)
  }

  if ("uri" %in% names(objetosPossiveis)) {

    objetosPossiveis <- objetosPossiveis %>%
      dplyr::select(-uri)

  }

  if ("uri" %in% names(proposicoesAfetadas)) {
    proposicoesAfetadas <- proposicoesAfetadas %>%
      dplyr::select(-uri)
  }

  if ("uri" %in% names(content)) {
    content <- content %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento))
  }


  dat <- list(
    votacao = content,
    objetosPossiveis = objetosPossiveis,
    proposicoesAfetadas = proposicoesAfetadas,
    ultimaApresentacaoProposicao = ultimaApresentacaoProposicao
  )

  dat

}

#' @title Get voting guideline for a political party on a given voting process
#'
#' @description
#' Sometimes political parties guide the vote of its representatives.
#' This function returns information on what were those guidelines for a given voting process
#'
#' @param id An unique identifier for a voting process
#'
#' @return A tibble of voting guideline for a political party on a given voting process
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_orientacoes(id = "2265603-43")
votacoes_orientacoes <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("votacoes/",id, "/orientacoes")

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop(paste0("404 Not Found. Couldn't find a voting process for id'",id,"'"), call. = FALSE)
    }
  )


  content <- req$dados
  not_zero_content(content)

  if ("uriPartidoBloco" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uriPartidoBloco)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information on representatives voting behavior for a given voting process
#'
#' @description
#' Get information on representatives behavior on roll-call votes at the Brazilian House of Representatives.
#' Note: There aren't recordings for absent representatives and the function will only return values for those that indeed vote.
#'
#' @param id  An unique identifier for a voting process
#'
#' @return A tibble on representatives voting behavior for a given voting process
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_votos(id = "2265603-43")
votacoes_votos <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("votacoes/",id, "/votos")

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop(paste0("404 Not Found. Couldn't find a voting process for id'",id,"'"), call. = FALSE)
    }
  )


  content <- req$dados
  not_zero_content(content)

  deputado <- content$deputado_
  content$deputado_ <- NULL

  content %>%
    dplyr::bind_cols(.,deputado) -> content

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }

}
