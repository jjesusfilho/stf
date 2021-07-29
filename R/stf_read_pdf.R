#' Reads pdf files
#'
#' @param files Vector of pdf files  to be read.
#' @param path  Path to pdf files if files are not informed
#' @return a tibble with tree columns: incidente, texto, and doc_id.
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_read_stf(files = "")
#' }
stf_read_pdf <- function(files = NULL, path = ".") {

  if (is.null(files)){

    files <- list.files(path, full.names = TRUE, pattern = "pdf$")

  }

  pb <- progress::progress_bar$new(total = length(files))

  purrr::map_dfr(files, purrr::possibly(~{

    pb$tick()

    texto <- suppressMessages(pdftools::pdf_text(.x)) %>%
      paste0(collapse = "")

    doc_id <- stringr::str_extract(.x, "(?<=docid_)\\d+")

    incidente <- stringr::str_extract(.x, "(?<=incidente_)\\d+")

    tibble::tibble(incidente = incidente, texto = texto, doc_id = doc_id)

  }, NULL))
}
