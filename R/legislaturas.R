#' @title GET a list of legislative periods
#'
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble with a list of legislative periods
#' @export
#' @family legislaturas
#' @examples
#' a <- legislaturas()
legislaturas <- function(...) {

  query_list <- list(...)

  req <- deputados_api("legislaturas",query_list)

  content <- req$dados

  tibble::as_tibble(content) %>%
    dplyr::select(-uri)

}

#' @title Get legislative period id
#'
#' @param date the year for checking an active legislative period id
#'
#' @return A legislative period id
#' @export
#' @family legislaturas
#' @examples
#' legislaturas_id(date = "2020-01-01")
legislaturas_id <- function(date) {

  date <- suppressWarnings(lubridate::ymd(date))

  tryCatch(
    assertthat::assert_that(!is.na(date)),
    error = function(e) {
      stop("Wrong date format for 'date'. Try 'YYYY-MM-DD'")
    }
  )

  if(!is.character(date)){
    date <- as.character(date)
  }

  id <- tryCatch(
    legislaturas(data = date)$id,
    error = function(e) {
      stop("404 Not found")
    }
  )

  id

}


#' @title Get legislative period information
#'
#' @param id legislative period id
#'
#' @return A tibble with legislative period information
#' @export
#' @family legislaturas
#' @examples
#' a <- legislaturas_info(id = 1)
legislaturas_info <- function(id) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("legislaturas/",id)
  req <- deputados_api(path)

  content <- req$dados

  tibble::as_tibble(content) %>%
    dplyr::select(-uri)

}


#' @title Get information of speakers for the House of Representative for a given legislative period
#'
#' @param id legislative period id
#' @param ... query parameters for the House of Representatatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)
#'
#' @return A tibble with speakers for the House of Representative for a given legislative period
#' @export
#' @family legislaturas
#' @examples
#' a <- legislaturas_mesa(id = 55)
legislaturas_mesa <- function(id,...) {

  if (is.numeric(id)) {
    id <- as.character(id)
  }

  query_list <- list(...)


  path <- paste0("legislaturas/",id, "/mesa")
  req <- deputados_api(path, query_list)

  content <- req$dados

  if (length(content) == 0) {

    warning("There is no data for this entry.", call. = FALSE)

  }

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri, uriPartido))

  } else {
    tibble::as_tibble(content)
  }

}
