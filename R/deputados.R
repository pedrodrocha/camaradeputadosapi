
#' Get a list of representatives (current and historical)
#'
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return
#' @export
#'
#' @examples
#' a <- deputados()
#' b <- deputados(siglaUF = "MG")
#' c <- deputados(siglaUF = "MG", siglaPartido = "PT")
deputados <- function(...) {

  query_list <- list(...)

  req <- deputados_api("deputados",query_list)

  tibble::as_tibble(req$dados) %>%
    dplyr::select(-c(uri,uriPartido))


}


#' Extract a representative Id
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' afonso_arinos <- deputados_getId(name = "Afonso Arinos")

deputados_id <- function(name) {

  id <- tryCatch(
    deputados(nome = name)$id,
    error = function(e) {
      stop("404 Not found")
    }
  )

  tryCatch(
    assertthat::assert_that(length(id) == 1),
    error = function(e) {
      stop("More than one Id found, can you be more specific?")
    }
  )

  id

}


#' GET general information of a representative
#'
#' @param id representative id
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return
#' @export
#'
#' @examples info_geral <- deputados_infoGeral(id = 204554)
deputados_info <- function(id, ...) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  path <- paste0("deputados/",id)
  req <- deputados_api(path,query_list)

  content <- req$dados

  content <- zero_or_null(content)

  ultimoStatus <- content$ultimoStatus
  content$ultimoStatus <- NULL


  ultimoStatus <- zero_or_null(ultimoStatus)


  gabinete <- ultimoStatus$gabinete
  ultimoStatus$gabinete <- NULL

  gabinete <- zero_or_null(gabinete)



  content <- tibble::as_tibble(content)
  gabinete <- tibble::as_tibble(gabinete) %>%
    dplyr::select(-nome, -email) %>%
    dplyr::rename(
      "gabinetePredio" = predio,
      "gabineteSala" = sala,
      "gabineteAndar" = andar,
      "gabineteTelefone" = telefone
    )
  ultimoStatus <- tibble::as_tibble(ultimoStatus)


  content %>%
    dplyr::left_join(.,ultimoStatus, by = c("id","uri")) %>%
    dplyr::select(-c(nome,uri,uriPartido)) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(.,gabinete)


}

#' GET representative expenditures while in office for a given year
#'
#' @param id
#' @param ...
#' @param year
#'
#' @return
#' @export
#'
#' @examples
deputados_despesas <- function(id, year, ...) {

  assertthat::has_args(deputados_despesas,args = year)

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  if(!is.numeric(year)){
    year <- as.numeric(year)
  }

  dat <- tibble::tibble()


  for (i in seq_along(year)) {
    query_list <- list(
      ano = year[i],
      ...
    )

    path <- paste0("deputados/",id,"/despesas")
    req <- deputados_api(path,query_list)

    content <- req$dados

    tryCatch(
      assertthat::assert_that(length(content) > 0),
      error = function(e) {
        stop("There are no entries for or more parameters you specified. Try again")
      }

    )

    dat <- dplyr::bind_rows(dat, content)

  }
  dat
}


#' GET representative speeches
#'
#' @param id
#' @param ...
#' @param from
#'
#' @return
#' @export
#'
#' @examples
deputados_discursos <- function(id, from, ...) {

  assertthat::has_args(deputados_discursos,args = from)

  from <- suppressWarnings(lubridate::ymd(from))

  tryCatch(
    assertthat::assert_that(!is.na(from)),
    error = function(e) {
      stop("Wrong date format for 'from'. Try 'YYYY-MM-DD'")
    }
  )

  if(!is.character(from)){
    from <- as.character(from)
  }

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(
    dataInicio = from,
    ...
  )

  path <- paste0("deputados/",id, "/discursos")
  req <- deputados_api(path,query_list)

  content <- req$dados

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}


#' GET a list of events the representative was present
#'
#' @param id
#' @param from
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
deputados_eventos <- function(id, from, ...) {
  assertthat::has_args(deputados_eventos, args = from)

  from <- suppressWarnings(lubridate::ymd(from))

  tryCatch(
    assertthat::assert_that(!is.na(from)),
    error = function(e) {
      stop("Wrong date format for 'from'. Try 'YYYY-MM-DD'")
    }
  )

  if(!is.character(from)){
    from <- as.character(from)
  }

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(
    dataInicio = from,
    ...
  )

  path <- paste0("deputados/",id, "/eventos")
  req <- deputados_api(path,query_list)

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


#' GET a list of parliamentary  fronts a representative is currently member
#'
#' @param id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
deputados_frentes <- function(id,...) {
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  path <- paste0("deputados/",id, "/frentes")
  req <- deputados_api(path,query_list)

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


#' GET a list of bodies and commissions a representative is member
#'
#' @param id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
deputados_orgaos <- function(id,...){

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  path <- paste0("deputados/",id, "/orgaos")
  req <- deputados_api(path,query_list)

  content <- req$dados

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uriOrgao)

  } else {
    tibble::as_tibble(content)
  }


}
