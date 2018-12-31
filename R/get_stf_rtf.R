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

  id <- stringr::str_extract(url,"\\d{3,}")

  purrr::map2_dfr(url,id,purrr::possibly(~{

    texto<-unrtf::unrtf(.x,"text") %>%
      stringr::str_remove("\\X+---------+") %>%
      stringr::str_squish()

    tibble::tibble(texto=texto,id=.y)

  },NULL))

}

