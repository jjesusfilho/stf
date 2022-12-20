#' Gets daily coummunication from STF about
#'    one's lawsuit
#'
#' @return tibble
#' @details You need to be authenticated `stf_authenticate`
#'     to use this function.
#' @export
#'
stf_get_communication <- function(){


  corpo <- '{"statusRecebimentoComunicacao":"NAO_LIDA","arrayModelosFiltro":[],"registrosPorPagina":100,"pagina":1,"camposDoFiltroDto":{"numeroUnico":false}}'


  url <- "https://sistemas.stf.jus.br/peticionamento/api/comunicacoes"

  resposta<- httr::POST(url, body = corpo,
                        httr::accept_json(),
                        httr::content_type_json())



  if (resposta$headers$`content-type` == "application/json;charset=UTF-8"){

    x <-    resposta |>
      httr::content("text") |>
      jsonlite::fromJSON(flatten = T)

  } else {

    x <- NA_character_

  }

  return(x)

}
