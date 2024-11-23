#' Baixa peças processuais a partir das urls lidas com stj_ler_tabela_pecas.R
#'
#' @param url_doc Vetor de urls
#' @param incidente Número do incidente
#' @param sequencia  Ordem das peças
#' @param diretorio Onde armazenar
#' @param sleep Ao menos um segundo entre requisições.
#'
#' @return pdfs
#' @export
#'
stf_baixar_pecas <- function(url_doc, incidente, sequencia, diretorio = ".", sleep = 1){
  
  purrr::pwalk(list(x = url_doc, y = incidente, z = sequencia), purrr::possibly(function(x,y,z){
    
    arquivo <- x |> 
        stringr::str_extract("(?<=\\?).+(?=#)") |> 
        stringr::str_replace_all("\\W","_") |> 
        paste0("_incidente_",y,"_sequencia_",z,".pdf") |> 
        file.path(diretorio, ... = _)
   
    
  httr::GET(x, httr::write_disk(arquivo),
            httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")
  )
    
  Sys.sleep(sleep)
  }), .progress = TRUE)
  
}
