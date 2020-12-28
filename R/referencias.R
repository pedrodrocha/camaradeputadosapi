#' @title Get metadata information on data related to the family 'deputados'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'deputados'
#' @export
#' @family referencias
#' @examples
#' a <- referencias_deputados()
referencias_deputados <- function(meta = c("siglaUF","siglaSituacao")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("siglaUF","siglaSituacao"),
      msg = "Unknow argument passed to 'meta'"
    )
  }


  path <- "referencias/deputados"
  req <- main_api(path = path)

  content <- req$dados

  content$siglaUF <- tibble::as_tibble(content$siglaUF)
  content$siglaSituacao <- tibble::as_tibble(content$siglaSituacao)


  content[meta]

}

#' @title Get metadata information on data related to the family 'eventos'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'eventos'
#' @export
#' @family referencias
#' @examples
#' a <- referencias_eventos()
referencias_eventos <- function(meta = c("codTipoEvento","codSituacao")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("codTipoEvento","codSituacao"),
      msg = "Unknow argument passed to 'meta'"
    )
  }

  path <- "referencias/eventos"

  req <- main_api(path = path)

  content <- req$dados

  content$codTipoEvento <- tibble::as_tibble(content$codTipoEvento)
  content$codSituacao <- tibble::as_tibble(content$codSituacao)


  content[meta]
}


#' @title Get metadata information on data related to the family 'orgaos'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'eventos'
#' @export
#' @family referencias
#' @examples
#' a <- referencias_orgaos()
referencias_orgaos <- function(meta = c("idTipoOrgao","idSituacao")) {

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

#' @title Get metadata information on data related to the family 'proposicoes'
#'
#' @param meta tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'proposicoes'
#' @export
#' @family referencias
#' @examples
#' a <- referencias_proposicoes()
referencias_proposicoes <- function(meta = c("siglaTipo","codSituacao","tiposTramitacao","codTema","codTipoAutor")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("siglaTipo","codSituacao","tiposTramitacao","codTema","codTipoAutor"),
      msg = "Unknow argument passed to 'meta'"
    )
  }


  path <- "referencias/proposicoes"

  req <- main_api(path = path)

  content <- req$dados

  content$siglaTipo <- tibble::as_tibble(content$siglaTipo)
  content$codSituacao <- tibble::as_tibble(content$codSituacao)
  content$tiposTramitacao <- tibble::as_tibble(content$tiposTramitacao)
  content$codTema <- tibble::as_tibble(content$codTema)
  content$codTipoAutor <- tibble::as_tibble(content$codTipoAutor)

  content[meta]
}
