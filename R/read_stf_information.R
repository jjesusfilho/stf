#' Reads data from information tab.
#'
#' @param files Vector of html files.
#' @param path where the htmls are if files is NULL.
#' @importFrom rlang .data
#' @return a tibble with data from the information tab.
#' @export
#'
#' @examples
#' \dontrun{
#' informacao <- read_stf_information("html", plan = "multiprocess")
#' }
read_stf_information <- function (files = NULL, path = ".")
{
  
  if (is.null(files)){
    files <- list.files(path, full.names = TRUE, pattern = "information")
  }
  
  pb <- progress::progress_bar$new(total = length(files))
  
  informacoes <-
    purrr::map_dfr(files, purrr::possibly( ~ {
      
      pb$tick()
      
      incidente <- stringr::str_extract(.x, "\\d+(?=.html)")
      
      conteudo <- xml2::read_html(.x, encoding = "UTF-8")
      
      
      if (xml2::xml_find_first(conteudo, "boolean(//body[contains(., 'Seu acesso a este website foi bloqueado')])")){
        
       data <-  tibble::tibble(
          incidente = incidente,
          assunto1 = "bloqueado",
          assunto2 = NA_character_,
          assunto3 = NA_character_,
          data_protocolo = NA,
          orgao_origem = NA_character_,
          origem = NA_character_,
          numero_origem = NA_character_,
          procedencia = NA_character_       
       )
      } else {
      
      
      assunto1 <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[1]"
        ) %>%
        xml2::xml_text()
      
      assunto2 <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[2]"
        ) %>%
        xml2::xml_text()
      
      assunto3 <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[3]"
        ) %>%
        xml2::xml_text(trim = TRUE)
      
      
      if (purrr::is_empty(assunto1)) {
        assunto1 <- NA_character_
      }
      
      
      if (purrr::is_empty(assunto2)) {
        assunto2 <- NA_character_
      }
      
      if (purrr::is_empty(assunto3)) {
        assunto3 <- NA_character_
      }
      
      
      data_protocolo <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='Data de Protocolo:']/following-sibling::div[1]"
        ) %>%
        xml2::xml_text(trim = TRUE)
      
      orgao_origem <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='\u00d3rg\u00e3o de Origem:']/following-sibling::div[1]"
        ) %>%
        xml2::xml_text(trim = TRUE) |>
        dplyr::na_if("")
      
      origem <-
        conteudo %>% xml2::xml_find_all("//div[normalize-space(text())='Origem:']/following-sibling::div[1]") %>%
        xml2::xml_text(trim = TRUE) |>
        dplyr::na_if("")
      
      numero_origem <-
        conteudo %>% xml2::xml_find_all(
          "//div[normalize-space(text())='N\u00famero de Origem:']/following-sibling::div[1]"
        ) %>%
        xml2::xml_text(trim = TRUE) |>
        dplyr::na_if("")
      
      procedencia <-
        conteudo %>% xml2::xml_find_all("//*[@id='descricao-procedencia']") %>%
        xml2::xml_text(trim = TRUE) |>
        dplyr::na_if("-")
      
    data <-  cbind(
        incidente = incidente,
        assunto1 = assunto1,
        assunto2 = assunto2,
        assunto3 = assunto3,
        data_protocolo = data_protocolo,
        orgao_origem = orgao_origem,
        origem = origem,
        numero_origem = numero_origem,
        procedencia = procedencia
      ) %>% tibble::as_tibble()
      
      }
      
      data
    }, NULL)) %>% 
    
    dplyr::select(
      incidente,
      assunto1,
      assunto2,
      assunto3,
      data_protocolo,
      orgao_origem,
      origem,
      numero_origem,
      procedencia
    ) %>% dplyr::mutate(data_protocolo = lubridate::dmy(data_protocolo))
}

#' @rdname read_stf_information
#' @export
stf_read_information <- read_stf_information



