#' Gets rtf texts from url connection
#'
#' @param url docs_url vector to rtf text.
#' @details This functions is a wrapper around download_stf_rtf
#'     and read_stf_rtf. It just returns a tibble with the
#'     text and the text id already parsed, without downloading it
#'     to your local disk.
#' @return a tibble
#' @export
#' @examples
#' \dontrun{
#' df <- get_stf_rtf()
#' }
get_stf_rtf <- function(url) {

  id <- stringr::str_extract(url, "\\d{3,}")

  pb <- progress::progress_bar$new(total = length(url))

  purrr::map2_dfr(url, id, purrr::possibly(~ {

    pb$tick()

    texto <- striprtf::read_rtf(.x) %>%
      stringr::str_remove("\\X+---------+") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("\\ue7e3","\\u00e7\\u00e3") %>%
      unlist() %>%
      stringr::str_c(collapse = "\n")


    tibble::tibble(texto = texto, id = .y)

  }, NULL))
}
