
#' Lê htmls baixados com stj_baixar_tabela_pecas.
#'
#' @param arquivos  Vetor de arquivos.
#' @param diretorio  Alternativamente, informar vetor de arquivos.
#'
#' @return tibble
#' @export
#'
stf_ler_tabela_pecas <- function(arquivos = NULL, diretorio = "."){
  
  if(is.null(arquivos)){
    
    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "html$")
    
  }
  
  purrr::map_dfr(arquivos, purrr::possibly(~{
    
    incidente <- stringr::str_extract(.x,"(?<=incidente_)\\d+")
    
    
    
    x <- .x |> 
      xml2::read_html()
    
    processo <- x |> 
      xml2::xml_find_all('//span[@class="PadraoOutputTextGrande"]') |> 
      xml2::xml_text() |> 
      purrr::pluck(2)
    
    url_doc <- x |> 
      xml2::xml_find_all("//td/a") |> 
      xml2::xml_attr("href")
    
    titulo <- x |> 
      xml2::xml_find_all("//td/a") |> 
      xml2::xml_attr("title")
    
    descricao <- x |> 
      xml2::xml_find_all("//td/a") |> 
      xml2::xml_text()
    
    tibble::tibble(incidente= incidente, processo = processo, titulo, descricao, url_doc) |> 
    dplyr::filter(stringr::str_detect(url_doc,"http")) |> 
    tibble::rownames_to_column("sequencia") |> 
    dplyr::mutate(proc_id = stringr::str_extract(url_doc,"(?<=prcID=)\\d+"),
                    id_doc = stringr::str_extract(url_doc,'(?<=docID=)\\d+'),
                    .after = incidente
      )
    
  }, NULL), .progress = TRUE)
  
  
}
