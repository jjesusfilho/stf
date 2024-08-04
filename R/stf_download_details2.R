#' Download details based on classe and docket number
#'
#' @param class This is what in Brazil is called classe processual
#' @param docket_number Number of the lawsuit
#' @param dir directory under which all subdirectories will
#'     be placed.
#'
#' @return Downloads the html and returns a vector of incident numbers
#' @export
#'
stf_download_details2 <- function(class = NULL, docket_number = NULL,dir = "."){
  
  urls <-  paste0(
    "http://portal.stf.jus.br/processos/listarProcessos.asp?classe=",
    class,
    "&numeroProcesso=",
    docket_number
  )
  
  
httr::set_config(httr::config(ssl_verifypeer = FALSE))

  incidente <-  purrr::map(urls,purrr::possibly(~{
    
    resposta <-  .x |> 
      httr::RETRY("GET",url = _,
                  httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")
                  
      )
    
    incidente <- resposta$url  |> 
      stringr::str_extract("\\d+$")
    
    arquivo <- paste0(dir,  format(Sys.Date(), "/date_%Y_%m_%d_incidente_"),incidente, ".html")
    
    writeBin(resposta$content,arquivo)
    
    incidente
    
  },NULL), .progress = TRUE)  |> 
    unlist()
  
  return(incidente)
}
