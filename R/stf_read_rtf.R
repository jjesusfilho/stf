#' Reads rtf files
#'
#' @param files of paths to the rtf files to be read.
#' @param path  Path to rtf files if files are not informed
#' @return a tibble with tree columns: incidente, texto, and doc_id.
#' @export
#'
#' @examples
#' \dontrun{
#' stf_read_rtf(files = "")
#' }
stf_read_rtf <- function(files = NULL, path = "." ) {

  if (is.null(file)){

    files <- list.files(path, full.names = TRUE, pattern = "rtf$")

  }



  pb <- progress::progress_bar$new(total = length(files))

  purrr::map_dfr(files, purrr::possibly(~{

    pb$tick()

    doc_id <- stringr::str_extract(.x, "(?<=docid_)\\d+")

    incidente <- stringr::str_extract(.x, "(?<=incidente_)\\d+")

    texto <- striprtf::read_rtf(.x) %>%
      stringr::str_remove("\\X+---------+") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("\\ue7e3","\\u00e7\\u00e3") %>%
      unlist() %>%
      stringr::str_c(collapse = "\n")


    tibble::tibble(incidente = incidente, texto = texto, doc_id = doc_id)
  }, NULL))
}
