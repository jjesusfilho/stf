#' Reads docket parties names.
#'
#' @param path where to find the htmls.
#' @param plan default to "sequential". See \code{future::plan}
#'     for all options.
#' @return Tibble with docket number, party name, and party class.
#' @export
#'
#' @examples
#' \dontrun{
#' partes<- read_stf_parties(path=".",plan="multiprocess")
#' }
#'
read_stf_parties <- function(path=".", plan="sequential"){

  arquivos <- list.files(path, pattern = ".html", full.names = TRUE)

  processos <- stringr::str_extract(arquivos,"\\d+(?=\\.html)")

  nomes <- tibble::tibble(

    partes = c("RECLTE", "ADV", "RECLDO", "INTDO", "PROC", "BENEF"),

    correcao = c("reclamante","advogado","reclamado","intimado","procurador","beneficiario")
  )


  future::plan(plan)

  partes <- furrr::future_map2_dfr(arquivos,processos,purrr::possibly(~{
    conteudo <- xml2::read_html(.x)

    coluna <- xml2::xml_find_all(conteudo,"//div[@class='detalhe-parte']") %>%
      xml2::xml_text() %>%
      unlist() %>%
      stringr::str_extract(".+?(?=\\.)") %>%
      pmatch(nomes$partes,duplicates.ok = TRUE) %>%
      nomes$correcao[.]

    parte_nome <-
      xml2::xml_find_all(conteudo,"//div[@class='nome-parte']") %>%
      xml2::xml_text() %>%
      iconv("UTF-8","latin1//TRANSLIT") %>%
      stringr::str_remove("&nbsp")

    tibble::tibble(docket=.y,party=coluna,party_name=parte_nome)

  },NULL))
}
