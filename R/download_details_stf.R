#' Download detailed information based on incidente number
#'
#' @param incidente incidente number
#' @param dir directory
#'
#' @return htmls
#' @export
#'
stf_download_details <- function(incidente =NULL, dir="."){


  uri<-"https://portal.stf.jus.br/processos/detalhe.asp?incidente="

  purrr::walk(incidente,purrr::possibly(purrrogress::with_progress(~{

    arquivo<-file.path(dir,paste0("detalhes_stf_incidente_",.x,".html"))

    url <- paste0(uri,.x)

    httr::GET(url,httr::write_disk(arquivo,overwrite = TRUE),
              httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")

              )

  }),NULL))


}
