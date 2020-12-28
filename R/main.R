
#' Main GET function for Brazilian lower house API
#'
#' @param path endpoint for api
#' @param args list of query arguments
#'
#' @return list
main_api <- function(path, args = NULL) {

  assertthat::assert_that(is.character(path))

  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/",path)

  if (length(args) == 0) {
    resp <- httr::GET(url,httr::accept("application/json"))

  } else {
    resp <- httr::GET(
      url,httr::accept("application/json"),
      query = args)
  }


  if (resp$status_code != 200) {
    stop(httr::http_status(resp)$message)
  }


  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json")

  }

  content <- jsonlite::fromJSON(httr::content(resp,"text"))

  content


}
