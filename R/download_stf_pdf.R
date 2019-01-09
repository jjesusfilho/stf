#' Downloads pdf text documents based on docket sheet
#'
#' @param sheet provide tibble read by function read_stf_sheet.
#' @param path  where to download the pdfs.
#'
#' @return downloaded pdfs with incident number and document id number.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' download_stf_pdf(sheet = andamento)
#' }
download_stf_pdf <- function(sheet, path = ".") {
  sheet <- sheet %>%
    dplyr::filter(stringr::str_detect(.data$docs_url, "pdf$")) %>%
    dplyr::select(.data$incidente, .data$docs_url)

  purrr::walk2(sheet$docs_url, sheet$incidente, purrr::possibly(~ {
    doc_id <- stringr::str_extract(.x, "\\d{3,}")

    httr::GET(.x, httr::write_disk(paste0(path, "/incidente_", .y, "_docid_", doc_id, ".pdf"), overwrite = TRUE))
  }, NULL))
}
