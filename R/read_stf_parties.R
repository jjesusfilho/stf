#' Reads docket parties names.
#'
#' @param files Paths to files.
#' @param path Where to find the htmls if files NULL.
#' @return Tibble with docket number, party name, and party class.
#' @export
#'
#' @examples
#' \dontrun{
#' partes <- read_stf_parties(path = ".")
#' }
#'
read_stf_parties <- function (files = NULL, path = ".")
{

  if (is.null(files)){

    files <- list.files(path, pattern = ".html", full.names = TRUE)


  }

  pb <- progress::progress_bar$new(total = length(files))


  partes <- purrr::map_dfr(files, purrr::possibly(~{

    pb$tick()

    incidente <- stringr::str_extract(.x, "\\d+(?=\\.html)")

    x <- xml2::read_html(.x)

    col <-
      xml2::xml_find_all(x, "//div[@class='detalhe-parte']") %>%
      xml2::xml_text() %>%
      unlist() %>%
      stringr::str_extract(".+?(?=\\.)")


    # pmatch(nomes$partes, duplicates.ok = TRUE) %>%
    # nomes$correcao[.]

    parte_nome <- xml2::xml_find_all(x, "//div[@class='nome-parte']") %>%
      xml2::xml_text() %>%
      iconv("UTF-8", "latin1//TRANSLIT") %>%
      stringr::str_remove("&nbsp")

    tibble::tibble(incidente = incidente,
                   parte = col,
                   parte_nome = parte_nome)
  }, NULL))
}
