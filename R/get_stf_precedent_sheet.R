#' Title Downloads STF precedent sheet
#'
#' @param url urls from precedent sheet.
#' @keywords stf, jurisprudência, andamento processual
#' @return tibble with the docket sheet from precedent search
#' @export
get_stf_precedent_sheet <- function(url) {
  xp <- "//table[@class='resultadoAndamentoProcesso']"
  url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = xp) %>%
    rvest::html_table() %>%
    magrittr::extract2(1)
}
