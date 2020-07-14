#' Downloads rtf documents based on doc_url column from docket sheet data.
#'
#' @param sheet provide tibble read by function read_stf_sheet.
#' @param path where to download the texts.
#' @details You don't need to inform which urls are rtfs.
#' @return files with rtf texts.
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_rtf(sheet = andamento)
#' }
download_stf_rtf <- function(sheet, path = ".") {

  sheet <- sheet %>%
    dplyr::filter(stringr::str_detect(doc_url, "RTF$")) %>%
    dplyr::select(incidente, doc_url)

  pb <- progress::progress_bar$new(total = nrow(sheet))

  purrr::walk2(sheet$doc_url, sheet$incidente, purrr::possibly(~ {

    pb$tick()

    doc_id <- stringr::str_extract(.x, "\\d{3,}")

    httr::GET(.x, httr::write_disk(paste0(path, "/incidente_", .y, "_docid_", doc_id, ".rtf"), overwrite = TRUE),
              httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")

              )
  }, NULL))
}
