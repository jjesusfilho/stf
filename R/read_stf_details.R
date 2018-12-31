#' Reads decision details
#'
#' @param path where to find the htmls
#' @param plan default to "sequential", see \code{future::plan},
#'     for all options.
#'
#' @return tibble with meio (public or eletronic), sigilo (secrecy),
#'     numero_unico (unique number), relator_atual (current justice)
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' detalhes <- read_stf_details(path=".",plan="multiprocess")
#' }
#'
read_stf_details <- function(path = ".", plan = "sequential") {
  files <- list.files(path, pattern = ".html", full.names = TRUE)

  incidentes <- stringr::str_extract(files, "\\d+(?=\\.html)")

  future::plan(plan)

  furrr::future_map2_dfr(files, incidentes, purrr::possibly(~{

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
      xml2::xml_find_all("//*[@class='processo-rotulo']") %>%
      xml2::xml_text() %>%
      stringr::str_extract("\\d.+")

    classe_numero <- content %>%
      xml2::xml_find_all("//input[@id='classe-numero-processo']") %>%
      xml2::xml_attr("value")

    relator_atual <-  content %>%
      xml2::xml_find_all("//div[contains(@class,'processo-dados')][2]") %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=: ).+")

    tibble::tibble(incidente = .y,
                   meio,
                   sigilo,
                   numero_unico,
                   classe_numero,
                   relator_atual)

  }, NULL), .progress = TRUE)


}
