
#' Title
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




#' Title
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
