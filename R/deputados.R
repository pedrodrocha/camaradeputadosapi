#' @title Get a list of representatives (current and historical)
#'
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble with a list of representatives
#' @export
#' @family deputados
#' @examples
#' a <- deputados()
#' b <- deputados(siglaUF = "MG")
#' c <- deputados(siglaUF = "MG", siglaPartido = "PT")
deputados <- function(...) {

  query_list <- list(...)

  req <- deputados_api("deputados",query_list)

  if (length(req$dados) == 0) {
    warning("There is no data for this entry.", call. = FALSE)
  }

  if ("uri" %in% names(req$dados)) {
    tibble::as_tibble(req$dados) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(req$dados)
  }


}


#' @title Extract a representative Id
#'
#' @param name The name of the representative you want to extract the id
#'
#' @return The id of the representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_id(name = "Afonso Arinos")
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


#' @title Get general information for a representative
#'
#' @param id representative id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return a tibble with general information for a representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_info(id = 204554)
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

#' @title Get representative expenditures while in office for a given year
#'
#' @param id representative id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#' @param year a year for checking the expenditures of a given representative
#'
#' @return a tibble of a representative expenditures while in office for a given year
#' @export
#' @family deputados
#' @examples
#' a <- deputados_despesas(id = deputados_id(name = "Eduardo Cunha"), 2013)
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


#' @title Get representative speeches
#'
#' @param id representative id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#' @param from the beginning date (YYYY-MM-DD) you want to collect speeches
#'
#' @return a tibble with speeches from the representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_discursos(id = deputados_id("Rodrigo Maia"), "2020-01-01")
deputados_discursos <- function(id, from, ...) {

  assertthat::has_args(deputados_discursos,args = from)

  from <- check_date(from)

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



#' Get a list of events the representative was present
#'
#' @param id A representative id
#' @param from the beginning date (YYYY-MM-DD) you want to collect events
#' @param to the end date (YYYY-MM-DD) you want to collect events
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble with events the representative was present
#' @export
#' @family deputados
#' @examples deputados_eventos(id = 74646, from = "2020-11-01", to = "2020-12-01")

deputados_eventos <- function(id, from, to, ...) {
  assertthat::has_args(deputados_eventos, args = c(from,to))

  from <- check_date(from)
  to <- check_date(to)

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(
    dataInicio = from,
    dataFim = to,
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




#' @title Get a list of parliamentary  fronts a representative is currently member
#' @param id representative id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return a tibble of parliamentary  fronts a representative is currently member
#' @export
#' @family deputados
#' @examples
#' a <- deputados_frentes(id = deputados_id("Marcelo Freixo"))
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


#' @title Get a list of bodies and commissions a representative is member
#' @param id the representative id
#' @param ... query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return a tibble with bodies and commissions a representative is member
#' @export
#' @family deputados
#' @examples
#' a <- deputados_orgaos(deputados_id("Alessandro Molon"))
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
