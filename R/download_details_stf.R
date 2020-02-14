#' Download detailed information based on incidente number
#'
#' @param incidente incidente number
#' @param dir directory
#'
#' @return htmls
#' @export
#'
download_details_stf <- function(incidente =NULL, dir="."){


  uri<-"https://portal.stf.jus.br/processos/detalhe.asp?incidente="

  purrr::walk(incidente,purrr::possibly(purrrogress::with_progress(~{

    arquivo<-file.path(dir,paste0("detalhes_stf_incidente_",.x,".html"))

    url <- paste0(uri,.x)
    httr::GET(url,httr::write_disk(arquivo,overwrite = TRUE))

  }),NULL))


}
