#' Reads rtf files
#'
#' @param file of paths to the rtf files to be read.
#' @param plan
#' @return a tibble with tree columns: incidente, texto, and doc_id.
#' @export
#'
#' @examples
#' \dontrun{
#' read_stf_rtf(file="")
#' }
read_stf_rtf <- function(file=NULL,plan="sequential"){

  future::plan(plan)

  furrr::future_map_dfr(file,purrr::possibly(~{

    doc_id <- stringr::str_extract(.x,"(?<=docid_)\\d+")

    incidente <- stringr::str_extract(.x,"(?<=incidente_)\\d+")

   texto <- unrtf::unrtf(.x,"text") %>%
      stringr::str_remove("\\X+---------+") %>%
      stringr::str_squish()

   tibble::tibble(incidente=incidente,texto=texto,doc_id=doc_id)
  },NA_character_))
}
