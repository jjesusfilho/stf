#' Download procedural documents (peças) list
#'
#' @param incidente Incident number
#' @param dir Directory where to put the htmls
#'
#' @return htmls to be read with stf_read_document_table
#' @export
#'
stf_download_document_table <- function(incidente, dir = "."){


  pb <- progress::progress_bar$new(total = length(incidente))

  uri <- "https://redir.stf.jus.br/estfvisualizadorpub/jsp/consultarprocessoeletronico/ConsultarProcessoEletronico.jsf?seqobjetoincidente="

  purrr::walk(incidente,purrr::possibly(~{

    pb$tick()

    arquivo <- file.path(dir, paste0("stf_table_incidente_",.x,".html"))

    url <- paste0(uri, .x)

    httr::GET(url,httr::write_disk(arquivo, overwrite = T))

  },NULL))


}
