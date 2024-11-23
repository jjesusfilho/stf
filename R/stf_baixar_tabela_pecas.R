#' Baixa tabela de peças processuais a partir do número do incidente.
#'
#' @param incidente Vetor de incidentes
#' @param diretorio Diretório onde armazenar os arquivos.
#'
#' @return html
#' @export
#'
stf_baixar_tabela_pecas <- function(incidente, diretorio = "."){
  
  if(any(stringr::str_detect(incidente,"\\D"))){
    
    stop("O incidente deve conter somente números")
    
  }
  
  purrr::walk(incidente, purrr::possibly(~{
    
    arquivo <- file.path(diretorio, paste0("tabela_docs_incidente_",.x,".html"))
    
    url_incidente <- paste0("https://redir.stf.jus.br/estfvisualizadorpub/jsp/consultarprocessoeletronico/ConsultarProcessoEletronico.jsf?seqobjetoincidente=", .x)
    
    httr::GET(url_incidente, httr::write_disk(arquivo, overwrite = T),
                     httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")
                     
    )
    
    
  }), .progress = TRUE)
  
}