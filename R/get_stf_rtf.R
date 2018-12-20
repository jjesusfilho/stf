#' Gets rtf texts from url connection
#'
#' @param x docs_url vector to rtf text.
#' @details This functions is wrapper around download_stf_rtf
#'     and read_stf_rtf. It just returns a tibble with the
#'     text and the text id already parsed, without downloading it
#'     to your local disk.
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' df <- get_stf_rtf()
#' }
get_stf_rtf<-function(x){
  purrr::map_chr(x,~{
    if (is.na(.x)){
      NA_character_
    } else {
      tmp<-tempfile(fileext = ".rtf")
      download.file(.x,tmp)
      unrtf::unrtf(tmp,"text") %>%
        stringr::str_remove("\\X+---------+") %>%
        stringr::str_squish()

    }
  })
}
