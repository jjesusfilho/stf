#' Downloads pdf text documents based on docket sheet
#'
#' @param sheet provide tibble read by function read_stf_sheet.
#' @param path  where to download the pdfs.
#'
#' @return downloaded pdfs with incident number and document id number.
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_pdf(sheet = andamento)
#' }
download_stf_pdf <- function(sheet, path = ".") {
  sheet <- sheet %>%
    dplyr::filter(stringr::str_detect(doc_url, "pdf$")) %>%
    dplyr::select(incidente, doc_url)

  purrr::walk2(sheet$doc_url, sheet$incidente, purrr::possibly(~ {
    doc_id <- stringr::str_extract(.x, "\\d{3,}")

    httr::GET(.x, httr::write_disk(paste0(path, "/incidente_", .y, "_docid_", doc_id, ".pdf"), overwrite = TRUE),
              httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")

              )
  }, NULL))
}
