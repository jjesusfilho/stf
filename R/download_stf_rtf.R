#' Downloads rtf documents based on docs_url column from follow-up data.
#'
#' @param x docs_url vector gotten from follow-up
#' @param path where to download the texts.
#'
#' @return files with rtf texts.
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_rtf(x=url)
#' }
download_stf_rtf <- function(x = NULL, path = ".") {
  ids <- stringr::str_extract(x, "\\d{3,}") %>%
    paste0(".rtf")

  purrr::walk2(x, ids, purrr::possibly( ~ {
    if (is.null(.x)) {
      stop("You have to inform at least one url")
    } else {
      httr::GET(.x, httr::write_disk(paste0(path, "/", .y), overwrite = TRUE))

    }
  }, NULL))

}
