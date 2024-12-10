#' Baixa html das partes com base no incidente
#'
#' @param incidente Número do incidente
#' @param dir Diretório onde armazenar os htmls
#' @param sono = Sono entre requisições.
#'
#' @return htmls
#' @export
#'
stf_baixar_partes <- function(incidente, dir = ".", sono = 1){


  purrr::walk(incidente,purrr::possibly(~{


    url <- paste0("http://portal.stf.jus.br/processos/abaPartes.asp?incidente=",.x)

    arquivo <- file.path(dir,paste0(format(Sys.Date(), "party_date_%Y_%m_%d_incidente_"), .x, ".html"))

    httr::RETRY("GET",url,httr::write_disk(arquivo),
                httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")

                )

    Sys.sleep(sono)

  },NULL), .progress = TRUE)

}
