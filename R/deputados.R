#' @title Get a list of representatives
#'
#' @description
#'
#' Returns a list of representatives that are or were in office in a period of time.
#' If query parameters from the API such as 'idLegislatura' or 'dataInicio' are not used, it will return only representatives currently in office.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#'
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#' @return A tibble with a list of representatives
#' @export
#' @family deputados
#' @examples
#' a <- deputados()
#' \donttest{
#' b <- deputados(siglaUF = "MG")
#' c <- deputados(siglaUF = "MG", siglaPartido = "PT")
#' }

deputados <- function(...) {

  query_list <- list(...)

  if (length(query_list) > 0) {
    check_api_parameters(names(query_list), query_list)

  }

  req <- main_api("deputados",query_list)

  content <- req$dados

  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri,uriPartido))

  } else {
    tibble::as_tibble(content)
  }


}


#' @title Extract a representative Id
#'
#' @param name A representative name
#'
#' @return The id of a representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_id(name = "Afonso Arinos")
deputados_id <- function(name) {

  assertthat::assert_that(!missing(name),msg = "'name' is missing")

  id <- deputados(nome = name)$id

  tryCatch(
    assertthat::assert_that(length(id) == 1),
    error = function(e) {
      stop("More than one Id found, can you be more specific?", call. = FALSE)
    }
  )

  id

}


#' @title Get general information from a representative
#'
#' @description
#' Returns general information from a given representative that in any moment in history held office at the Brazilian House of Representatives
#'
#' @param id An unique identifier for a given representative
#'
#' @return A tibble with general information for a representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_info(id = 204554)
deputados_info <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")

  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("deputados/",id)
  req <- main_api(path)

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

#' @title Get a representative expenditures while in office for a given year
#'
#' @description
#' Provides access to payments and reimbursements made by the House of Representatives on behalf of a representative for given a year.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param id An unique identifier for a given representative
#' @param ... Query parameters for the Brazilian House of Representatives API
#' @param year A given year for checking expenditures
#'
#' @return a tibble of a representative expenditures while in office for a given year
#' @export
#' @family deputados
#' @examples
#' a <- deputados_despesas(id = 74173, 2013)
#' \donttest{
#' b <- deputados_despesas(
#' id = 74173,
#' year = 2013,
#' cnpjCpfFornecedor = "06278433000190"
#' )
#' }
deputados_despesas <- function(id, year, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  assertthat::assert_that(!missing(year),msg = "'year' is missing")

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
    check_api_parameters(names(query_list),query_list)
    max_limit100(names(query_list),query_list)

    path <- paste0("deputados/",id,"/despesas")
    req <- main_api(path,query_list)

    content <- req$dados

    not_zero_content(content)

    dat <- dplyr::bind_rows(dat, content)

  }
  dat
}


#' @title Get a representative speeches
#'
#' @description
#'
#' Get speeches by a given representative that were officially registered at the Brazilian House of Representatives.
#' By default it returns 15 results per request with a max limit of 100 results. You can control the number of results with the API parameter 'itens'.
#' If query parameters from the API such as 'dataInicio', 'dataFim' or 'idLegislatura' are not used, it will return only speeches registered in the last 7 days.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#'
#' @param id An unique identifier for a given representative
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#'
#' @return a tibble with speeches from the representative
#' @export
#' @family deputados
#' @examples
#' a <- deputados_discursos(id = 74693, dataInicio = "2020-12-01")
#' \donttest{
#' b <- deputados_discursos(id = 74693, idLegislatura = '55', itens = 100)
#' }
deputados_discursos <- function(id, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")


  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  max_limit100(names(query_list),query_list)
  check_api_parameters(names(query_list),query_list)

  path <- paste0("deputados/",id, "/discursos")
  req <- main_api(path,query_list)

  content <- req$dados

  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}



#' @title  Get a list of events a representative was present
#'
#'
#' @description
#'
#' Get a list of events a representative was present in a given period of time.
#' By default it returns 15 results per request with a max limit of 100 results. You can control the number of results with the API parameter 'itens'.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param id An unique identifier for a given representative
#' @param from The beginning date (YYYY-MM-DD) you want to collect events
#' @param to The end date (YYYY-MM-DD) you want to collect events
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#' @return A tibble with events a representative was present
#' @export
#' @family deputados
#' @examples
#' a <- deputados_eventos(id = 74646, from = "2020-11-01", to = "2020-12-01")
#' \donttest{
#' b <- deputados_eventos(
#' id = 74160,
#' from = "2014-01-01",
#' to = "2020-12-01"
#' )
#' }

deputados_eventos <- function(id, from, to, ...) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  assertthat::assert_that(!missing(from),msg = "'from' is missing")
  assertthat::assert_that(!missing(to),msg = "'to' is missing")

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
  max_limit100(names(query_list),query_list)
  check_api_parameters(names(query_list),query_list)


  path <- paste0("deputados/",id, "/eventos")
  req <- main_api(path,query_list)

  content <- req$dados

  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }


}




#' @title Get a list of parliamentary fronts for a representative
#'
#' @description
#'
#' Get a list o basic information about parliamentary fronts a representative is currently member or was a member at the end of legislative period.
#'
#' @param id An unique identifier for a given representative
#'
#' @return A tibble of parliamentary  fronts a representative is currently member or was a member at the end of a legislative period
#' @export
#' @family deputados
#' @examples
#' a <- deputados_frentes(id = 76874)
deputados_frentes <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")

  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("deputados/",id, "/frentes")
  req <- main_api(path)

  content <- req$dados

  not_zero_content(content)


  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }
}


#' @title Get a list of bodies and commissions a representative is member
#'
#' @description
#'
#' Get a list of bodies and commissions a representative is currently member or was a member in a given period o time.
#' If query parameters from the API such as 'dataInicio' or 'dataFim' are not used, it will return only bodies or commissions a representative is currently member.
#' For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
#'
#' @param id An unique identifier for a given representative
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#' @return A tibble with bodies and commissions a representative is member
#' @export
#' @family deputados
#' @examples
#' a <- deputados_orgaos(160511)
deputados_orgaos <- function(id,...){

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)

  if (length(query_list) > 0) {
    check_api_parameters(names(query_list),query_list)
  }

  path <- paste0("deputados/",id, "/orgaos")
  req <- main_api(path,query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uriOrgao" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uriOrgao)

  } else {
    tibble::as_tibble(content)
  }


}


#' @title Get metadata information on data related to 'deputados'
#'
#' @description
#'
#' Get metadata information of data related to 'deputados'. Two types of metadata are available: a) 'siglaUF', with a list of Brazilian states; b) 'siglaSituacao', with a list of status for representatives.
#'
#' @param meta Tables of metadata for querying
#'
#' @return A list of tibbles with metadata information on data related to the family 'deputados'
#' @export
#' @family deputados
#' @examples
#' a <- deputados_referencias()
deputados_referencias <- function(meta = c("siglaUF","siglaSituacao")) {

  for (i in seq_along(meta)) {
    assertthat::assert_that(
      meta[i] %in% c("siglaUF","siglaSituacao"),
      msg = "Unknow argument passed to 'meta'"
    )
  }


  path <- "referencias/deputados"
  req <- main_api(path = path)

  content <- req$dados

  not_zero_content(content)

  content$siglaUF <- tibble::as_tibble(content$siglaUF)
  content$siglaSituacao <- tibble::as_tibble(content$siglaSituacao)


  content[meta]

}
