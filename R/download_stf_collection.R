#' Downloads Brazilian Supreme Courts decisions' collection
#'
#' @param decision_type either "monocraticas", "presidente" or "colegiadas"
#' @param years numeric vector of years
#' @param dir directory where to download the files. Defalts to the current dir.
#'
#' @return downloaded files with metadata of all supreme court's decisions
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_colletion(decision_type = "monocraticas", years = c(2017, 2018), path = ".")
#' }
download_stf_collection <- function(decision_type = NULL, years = NULL, dir = ".") {
  if (!is.numeric(years)) {
    stop("years must be numeric")
  }

  urls <- switch(decision_type,
    colegiadas = sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/colegiadas/decisoes_colegiadas_geral_lista_%d.xlsx", years),
    monocraticas = sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/decisoes_monocraticas_lista_%d.xlsx", years),
    presidente = sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/decisoes_monocraticas_lista_presidente_%d.xlsx", years)
  )

  files <- stringr::str_extract(urls, "decisoes_.+(?=\\.)") %>%
    stringr::str_c(dir, "/", ., ".xlsx")

  purrr::walk2(urls, files, ~ httr::GET(.x, httr::write_disk(.y, overwrite = T)))
}
