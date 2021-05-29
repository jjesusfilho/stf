#' Reads Brazilian Supreme Court decision's docket sheet.
#'
#' @param files Vector of html files downloaded by download_docket
#' @param path Where to find the htmls if files is NULL
#'
#' @return tibble with follow-up details
#' @export
#'
#' @examples
#' \dontrun{
#' andamento <- read_stf_docket_sheet(path = ".")
#' }
#'
read_stf_docket_sheet <- function(files = NULL, path = ".") {



  if(is.null(files)){

  files <- list.files(path, full.names = TRUE, pattern = "sheet")

  }

 pb <- progress::progress_bar$new(total = length(files))

  purrr::map_dfr(files,  purrr::possibly(~{

    pb$tick()

    incidente <- stringr::str_extract(.x, "\\d+(?=.html)")

    item <- xml2::read_html(.x, encoding = "UTF-8") %>%
      xml2::xml_find_all("//div[@class='andamento-item']")

    data_andamento <- item %>%
      purrr::map(~ xml2::xml_find_first(.x, ".//div[contains(@class,'andamento-data')]") %>%
        xml2::xml_text(trim = TRUE)) %>%
      unlist() %>%
      as.Date(format = "%d/%m/%Y")

    titulo <- item %>%
      purrr::map(~ xml2::xml_find_first(.x, ".//h5[contains(@class,'andamento-nome')]") %>%
        xml2::xml_text(trim = TRUE)) %>%
      unlist()

    invalido <- item %>%
      purrr::map(~ xml2::xml_find_first(.x, "boolean(.//h5[contains(@class,'andamento-invalido')])")) %>%
      unlist()

    descricao <- item %>%
      purrr::map(~ xml2::xml_find_first(.x, ".//div[contains(@class,'col-md-9 p-0')]") %>%
        xml2::xml_text(trim = TRUE)) %>%
      unlist()

    orgao_julgador <- item %>%
      purrr::map(~ xml2::xml_find_first(.x, ".//div/span[contains(@class,'andamento-julgador')]") %>%
        xml2::xml_text(trim = TRUE)) %>%
      unlist()


    doc <- item %>%
      purrr::map(~ xml2::xml_find_all(.x, ".//div[contains(@class,'andamento-docs')]/a") %>%
        xml2::xml_text() %>%
        stringr::str_squish(), .default = NA) %>%
      purrr::map_if(rlang::is_empty, ~NA_character_)

    doc_url <- item %>%
      purrr::map(~ xml2::xml_find_all(.x, ".//div[contains(@class,'andamento-docs')]/a/@href") %>%
        xml2::xml_text() %>%
        xml2::url_absolute("http://portal.stf.jus.br/processos/")) %>%
      purrr::map_if(rlang::is_empty, ~NA_character_)

    doc_id <- stringr::str_extract(doc_url, "\\d{3,}")

    tibble::tibble(incidente = incidente, data_andamento, titulo, invalido, orgao_julgador, descricao, doc, doc_url, doc_id) %>%
      tidyr::unnest() %>%
      dplyr::distinct()

  }, NULL))
}
