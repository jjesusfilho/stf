#' Reads decision details
#'
#' @param files Files with full path.
#' @param path Where to find the htmls
#'
#' @return tibble with meio (public or eletronic), sigilo (secrecy),
#'     numero_unico (unique number), relator_atual (current justice)
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' detalhes <- read_stf_details(path = ".")
#' }
#'
read_stf_details <- function(files = NULL, path = ".") {
  
  if(is.null(files)){
    
    files <- list.files(path, pattern = ".html", full.names = TRUE)
    
  }
  
  pb <- progress::progress_bar$new(total = length(files))
  
  purrr::map_dfr(files, purrr::possibly(~ {
    
    pb$tick()
    
    incidente <- stringr::str_extract(.x, "\\d+(?=\\.html)")
    
    
    content <- xml2::read_html(.x)
    
    
    
    meio <- content %>%
      xml2::xml_find_all("//*[contains(@class,'badge')]") %>%
      xml2::xml_text() %>%
      .[1]
    
    sigilo <- content %>%
      xml2::xml_find_all("//*[contains(@class,'badge')]") %>%
      xml2::xml_text() %>%
      .[2]
    
    numero_unico <- content %>%
      xml2::xml_find_first("//*[@class='processo-rotulo']") %>%
      xml2::xml_text() %>%
      stringr::str_extract("\\d.+")
    
    classe_numero <- content %>%
      xml2::xml_find_first("//input[@id='classe-numero-processo']") %>%
      xml2::xml_attr("value")
    
    classe_processo <- content |> 
      xml2::xml_find_first("//div[@class='processo-classe p-t-8 p-l-16']") |> 
      xml2::xml_text(trim = T)
    
    relator_atual <- content %>%
      xml2::xml_find_all("//div[contains(@class,'processo-dados')][2]") %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=: ).+")
    
    tibble::tibble(
      incidente = incidente,
      meio,
      sigilo,
      numero_unico,
      classe_processo,
      classe_numero,
      relator_atual
    ) %>%
      tidyr::separate(classe_numero, c("classe", "numero"), " ")
  }, NULL))
}
