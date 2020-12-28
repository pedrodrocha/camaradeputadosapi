#' @title Get a list of voting processes
#'
#' @param ... query parameters for the House of Representatives API (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info})
#'
#' @return A tibble of voting processes
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes()
votacoes <- function(...) {

  query_list <- list(...)

  req <- main_api("votacoes",query_list)

  content <- req$dados

  if (length(content) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriOrgao,uriEvento,uriProposicaoObjeto))

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get detailed information on voting processes
#'
#' @param id An unique identifier for a voting process
#'
#' @return A list of tibbles with detailed information on voting processes
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_info(id = "2265603-43")
votacoes_info <- function(id) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }



  path <- paste0("votacoes/",id)

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 Not Found. There is no data for this entry", call. = FALSE)
    }
  )


  content <- req$dados

  if (length(content) == 0) {
    stop("There is no data for this entry", call. = FALSE)
  }

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
#' @param id An unique identifier for a voting process
#'
#' @return A tibble of voting guideline for a political party on a given voting process
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_orientacoes(id = "2265603-43")
votacoes_orientacoes <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("votacoes/",id, "/orientacoes")

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 Not Found. There is no data for this entry", call. = FALSE)
    }
  )


  content <- req$dados

  if ("uriPartidoBloco" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uriPartidoBloco)

  } else {
    tibble::as_tibble(content)
  }
}

#' @title Get information on representatives voting behavior for a given voting process
#'
#' @param id  An unique identifier for a voting process
#'
#' @return A tibble on representatives voting behavior for a given voting process
#' @export
#' @family votacoes
#' @examples
#' a <- votacoes_votos(id = "2265603-43")
votacoes_votos <- function(id) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  path <- paste0("votacoes/",id, "/votos")

  req <- tryCatch(
    main_api(path),
    error = function(e) {
      stop("404 Not Found. There is no data for this entry", call. = FALSE)
    }
  )

  content <- req$dados

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
