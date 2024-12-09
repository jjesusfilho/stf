#' Estima quantidade de processos no STF
#'
#' @param inicio Número inicial
#' @param fim Número final

#' @return Quantidade distribuída
#' @export
#'
stf_quantidade <- function (inicio = NULL, fim = NULL) {
  
  
  while (fim - inicio > 7) {
    
    inicio <- mean(c(inicio, fim))
    
    intervalo <- round(inicio + -7:7) |>  range()
    
    urls <-   paste0("https://portal.stf.jus.br/processos/abaInformacoes.asp?incidente=", intervalo[1]:intervalo[2])
    
     n <- purrr::map(urls, ~{
      
     r1 <-  httr::GET(.x,   
                    httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")) |> 
          httr::content(encoding = "UTF-8") |> 
            xml2::xml_find_first(
              "//div[normalize-space(text())='Data de Protocolo:']/following-sibling::div[1]"
            ) |> 
            xml2::xml_text(trim = TRUE) |> 
            stringr::str_subset("^$", negate = T)
     
      #Sys.sleep(1) 
      r1
    }) |> 
      unlist()
    
    
    if (length(n) == 0) {
      inicio <- inicio - (fim - inicio)
      fim <- mean(c(inicio, fim))
    }
  }
  return(inicio)
}