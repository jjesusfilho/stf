#' Reads rtf files
#'
#' @param files of paths to the rtf files to be read.

#' @return a tibble with tree columns: incidente, texto, and doc_id.
#' @export
#'
#' @examples
#' \dontrun{
#' read_stf_rtf(files = "")
#' }
read_stf_rtf <- function(files = NULL) {

  pb <- progress::progress_bar$new(total = length(files))

  purrr::map_dfr(files, purrr::possibly(~{

    pb$tick()

    doc_id <- stringr::str_extract(.x, "(?<=docid_)\\d+")

    incidente <- stringr::str_extract(.x, "(?<=incidente_)\\d+")

    texto <- unrtf::unrtf(.x, "text") %>%
      stringr::str_remove("\\X+---------+") %>%
      stringr::str_squish()

    tibble::tibble(incidente = incidente, texto = texto, doc_id = doc_id)
  }, NULL))
}
