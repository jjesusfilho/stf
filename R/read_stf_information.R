#' Title
#'
#' @param path where the htmls are.
#' @param plan default to "sequential". See \code{future::plan} for
#'     more options.
#'
#' @return a tibble with data from the information tab.
#' @export
#'
#' @examples
#' \dontrun{
#' informacao<-read_stf_information("html",plan="multiprocess")
#' }
read_stf_information <- function(path = ".", plan="sequential") {
  
  files <- list.files(path, full.names = TRUE)
  
  incidentes <- stringr::str_extract(files, "\\d{3,}")
  
  future::plan(plan)
  
  informacoes <- furrr::future_map2_dfr(files, incidentes, purrr::possibly( ~{
    
    conteudo <- xml2::read_html(.x, encoding = "UTF-8")
    
    
    # ramo_direito <- conteudo %>%
    # 
    #   xml2::xml_find_all("//div[normalize-space(text())='Ramo do Direito:']/following-sibling::div") %>%
    #   xml2::xml_text(trim = TRUE)
    
    assunto1 <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[1]") %>%
      xml2::xml_text()
    
    assunto2 <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[2]") %>%
      xml2::xml_text()
    
    assunto3 <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Assunto:']/following-sibling::div/ul/li[3]") %>%
      xml2::xml_text(trim=TRUE)
    
    data_protocolo <- conteudo %>%
      xml2::xml_find_all(
        "//div[normalize-space(text())='Data de Protocolo:']/following-sibling::div[1]"
      ) %>%
      xml2::xml_text() %>%
      lubridate::dmy()
    
    orgao_origem <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Órgão de Origem:']/following-sibling::div[1]") %>%
      xml2::xml_text(trim=TRUE)
    
    origem <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Origem:']/following-sibling::div[1]") %>%
      xml2::xml_text(trim=TRUE)
    
    numero_origem <- conteudo %>%
      xml2::xml_find_all("//div[normalize-space(text())='Número de Origem:']/following-sibling::div[1]") %>%
      xml2::xml_text(trim=TRUE)
    
    procedencia <- conteudo %>%
      xml2::xml_find_all("//*[@id='descricao-procedencia']") %>%
      xml2::xml_text(trim = TRUE)
    
    
    cbind(
      incidente=.y,
      ramo_direito=ramo_direito,
      assunto1=assunto1,
      assunto2=assunto2,
      assunto3=assunto3,
      data_protocolo=data_protocolo,
      orgao_origem=orgao_origem,
      origem=origem,
      numero_origem=numero_origem,
      procedencia=procedencia
    ) %>%
      tibble::as_tibble()
    
    
  }, NULL), .progress = TRUE) %>%
    dplyr::select(incidente,assunto1,assunto2,assunto3,data_protocolo,
                  orgao_origem,origem,numero_origem,procedencia)
  
  
}
