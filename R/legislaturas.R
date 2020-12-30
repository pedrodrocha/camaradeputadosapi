#' @title Get a list of legislative periods
#'
#' @description
#'
#' Get a complete list of legislative periods for the Brazilian House of Representatives since 1826.
#'
#' @param ... Query parameters for the Brazilian House of Representatives API
#'
#' @return A tibble with a list of legislative periods
#' @export
#' @family legislaturas
#' @examples
#' a <- legislaturas()
legislaturas <- function(...) {

  query_list <- list(..., itens = 100)

  req <- main_api("legislaturas",query_list)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-uri)

  } else {
    tibble::as_tibble(content)
  }

}

#' @title Extract a legislative period id
#'
#' @param date A date (YYYY-MM-DD) for checking a legislative period id
#'
#' @return A legislative period id
#' @export
#' @family legislaturas
#' @examples
#' legislaturas_id(date = "2020-01-01")
legislaturas_id <- function(date) {
  assertthat::assert_that(!missing(date),msg = "'date' is missing")
  date <- check_date(date)

  id <- tryCatch(
    legislaturas(data = date)$id,
    error = function(e) {
      stop(paste0("404 Not found. There isn't an active legislative period for '",date,"'"), call. = FALSE)
    }
  )

  as.character(id)

}


#' @title Get information on speakers of the House of Representative for a given legislative period
#'
#' @description
#'
#' Get information on speakers of the House of Representatives for a given legislative period.
#' Usually each legislative period have two groups of speakers.
#'
#' @param id An unique identifier for a legislative period
#'
#' @return A tibble with speakers of the House of Representative for a given legislative period
#' @export
#' @family legislaturas
#' @examples
#' a <- legislaturas_mesa(id = 55)
legislaturas_mesa <- function(id) {

  assertthat::assert_that(!missing(id),msg = "'id' is missing")
  if (is.numeric(id)) {
    id <- as.character(id)
  }


  path <- paste0("legislaturas/",id, "/mesa")
  req <- main_api(path)

  content <- req$dados
  not_zero_content(content)

  if ("uri" %in% names(content)) {
    tibble::as_tibble(content) %>%
      dplyr::select(-c(uri, uriPartido))

  } else {
    tibble::as_tibble(content)
  }

}
