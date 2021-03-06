#' @title Check date
#'
#' @param date a date entry for check
#'
#' @return a date that has been checked
check_date <- function(date) {
  date <- suppressWarnings(lubridate::ymd(date))

  tryCatch(
    assertthat::assert_that(!is.na(date)),
    error = function(e) {
      stop("Wrong date format. Try 'YYYY-MM-DD'")
    }
  )

  if(!is.character(date)){
    date <- as.character(date)
  }

  date
}

#' @title Check official API parameters
#'
#' @param param API parameters name
#' @param query_list API parameters list
check_api_parameters <- function(param, query_list) {

  if (is.element("dataInicio",param)) {
    date <- suppressWarnings(lubridate::ymd(query_list['dataInicio']))

    tryCatch(
      assertthat::assert_that(!is.na(date)),
      error = function(e) {
        stop("Wrong date format for 'dataInicio'. Try 'YYYY-MM-DD'", call. = FALSE)
      }
    )

  }


  if (is.element("dataFim",param)) {
    date <- suppressWarnings(lubridate::ymd(query_list['dataFim']))

    tryCatch(
      assertthat::assert_that(!is.na(date)),
      error = function(e) {
        stop("Wrong date format for 'dataFim'. Try 'YYYY-MM-DD'", call. = FALSE)
      }
    )
  }


  if (is.element("dataApresentacaoInicio",param)) {
    date <- suppressWarnings(lubridate::ymd(query_list['dataApresentacaoInicio']))

    tryCatch(
      assertthat::assert_that(!is.na(date)),
      error = function(e) {
        stop("Wrong date format for 'dataApresentacaoInicio'. Try 'YYYY-MM-DD'", call. = FALSE)
      }
    )
  }

  if (is.element("dataApresentacaoFim",param)) {
    date <- suppressWarnings(lubridate::ymd(query_list['dataApresentacaoFim']))

    tryCatch(
      assertthat::assert_that(!is.na(date)),
      error = function(e) {
        stop("Wrong date format for 'dataApresentacaoFim'. Try 'YYYY-MM-DD'", call. = FALSE)
      }
    )
  }



  if (is.element("siglaSexo",param)) {

    tryCatch(
      assertthat::assert_that(is.element(query_list['siglaSexo'], c("M","F"))),
      error = function(e) {
        stop("Invalid argument for 'siglaSexo", call. = FALSE)
      }
    )

  }

  if (is.element("cnpjCpfFornecedor", param)){

    value <- unlist(query_list['cnpjCpfFornecedor'])

    tryCatch(
      assertthat::assert_that(is.character(value)),
      error = function(e) {
        stop("'cnpjCpfFornecedor' should be a character vector", call. = FALSE)
      }
    )
  }

  if (is.element("idLegislatura", param)){

    value <- unlist(query_list['idLegislatura'])

    tryCatch(
      assertthat::assert_that(is.character(value)),
      error = function(e) {
        stop("'idLegislatura' should be a character vector", call. = FALSE)
      }
    )
  }

}

#' @title Check for contents of length 0
#'
#' @param content query request content
not_zero_content <- function(content) {
  tryCatch(
    assertthat::assert_that(length(content) > 0),
    error = function(e) {
      stop("There is no data for this entry", call. = FALSE)
    }

  )
}

#' @title Check max limit of requests(100)
#'
#' @param param API parameters name
#' @param query_list API parameters list
max_limit100 <- function(param, query_list) {

  if (is.element("itens",param)) {
    if(query_list['itens'] > 100){
      warning("The number of itens requested exceeded the max limit and the query will return a maximum 100 results", call. = FALSE)
    }

  }

}

#' @title Check max limit of requests(200)
#'
#' @param param API parameters name
#' @param query_list API parameters list
max_limit200 <- function(param, query_list) {

  if (is.element("itens",param)) {
    if(query_list['itens'] > 200){
      warning("The number of itens requested exceeded the max limit and the query will return a maximum of 200 results", call. = FALSE)
    }

  }

}


