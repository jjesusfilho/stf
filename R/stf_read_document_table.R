#' Read procedutural document list table
#'     donwloaded with stf_download_document_table
#'
#' @param files Vector of htmls files
#' @param path Inform if files not provided
#'
#' @return Tibble
#' @export
#'
stf_read_document_table <- function(files = NULL, path){

  if (is.null(files)){

    files <- list.files(dir, full.names = TRUE, pattern = "html$")

  }

  pb <- progress::progress_bar$new(total = length(files))

  purrr::map_dfr(files, purrr::possibly(~{

    pb$tick()

    incidente <- stringr::str_extract(.x, "(?<=incidente_)\\d+")

    x <- xml2::read_html(.x)

    titulo <- x |>
        xml2::xml_find_all("//a[@target='imgDocumento']") |>
        xml2::xml_attr("title")

    url <- x |>
      xml2::xml_find_all("//a[@target='imgDocumento']") |>
      xml2::xml_attr("href")

    descricao <- x |>
      xml2::xml_find_all("//a[@target='imgDocumento']") |>
      xml2::xml_text()

    tibble::tibble(incidente, titulo, descricao, url)


  },NULL))

}
