#' Download detailed information based on incidente number
#'
#' @param incidente incidente number
#' @param dir directory
#'
#' @return htmls
#' @export
#'
stf_download_details <- function(incidente =NULL, dir="."){


  h <- httr::add_headers(`User-Agent`= "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36")


  uri<-"https://portal.stf.jus.br/processos/detalhe.asp?incidente="

  pb <- progress::progress_bar$new(total = length(incidente))

  purrr::walk(incidente,purrr::possibly(~{

    pb$tick()

    arquivo<-file.path(dir,paste0("detalhes_stf_incidente_",.x,".html"))

    url <- paste0(uri,.x)

    httr::GET(url, h, httr::write_disk(arquivo,overwrite = TRUE))


  },NULL))


}
