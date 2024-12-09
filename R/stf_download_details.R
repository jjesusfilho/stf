#' Baixa detalhes do capa com base no número do incidente
#'
#' @param incidente Número do incidente
#' @param dir diretório
#' @param sono = Sono entre requisições.
#'
#' @return htmls
#' @export
#'
stf_baixar_detalhes <- function(incidente =NULL, dir=".", sono = 1){


  h <- httr::add_headers(`User-Agent`= "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36")


  uri<-"https://portal.stf.jus.br/processos/detalhe.asp?incidente="


  purrr::walk(incidente,purrr::possibly(~{


    arquivo<-file.path(dir,paste0("detalhes_stf_incidente_",.x,".html"))

    url <- paste0(uri,.x)

    httr::GET(url, h, httr::write_disk(arquivo,overwrite = TRUE))

  Sys.sleep(sono)

  },NULL), .progress = TRUE)

}
