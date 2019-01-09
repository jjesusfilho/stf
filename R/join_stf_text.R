#' Joins pdf and rtf texts to docket sheet
#'
#' @param sheet docket sheet used to download the texts with
#'     functions download_stf_rtf and download_stf_pdf.
#' @param rtf tibble with rtf texts
#' @param pdf tibble with pdf texts
#' @importFrom rlang .data
#'
#' @return new tibble with all texts.
#' @export
#'
#' @examples
#' \dontrun{
#' andamento <- join_text(sheet = andamento, rtf = texto_rtf, texto_pdf)
#' }
join_stf_text <- function(sheet, rtf = NULL, pdf = NULL) {
  if (is.null(rtf) & is.null(rtf)) {
    stop("You have to provide at least one text tibble")
  } else if (is.null(rtf)) {
    sheet <- sheet %>%
      dplyr::right_join(pdf, by = c("doc_id", "incidente"))
  } else if (is.null(pdf)) {
    sheet <- sheet %>%
      dplyr::right_join(rtf, by = c("doc_id", "incidente"))
  } else {
    sheet <- sheet %>%
      dplyr::right_join(pdf, by = c("doc_id", "incidente")) %>%
      dplyr::right_join(rtf, by = c("doc_id", "incidente")) %>%
      dplyr::mutate(
        texto = dplyr::coalesce(.data$texto.x, .data$texto.y),
        doc_id = dplyr::coalesce(.data$doc_id.x, .data$doc_id.y)
      ) %>%
      dplyr::select(-c(.data$texto.x, .data$texto.y, .data$doc_id.x, .data$doc_id.y))
  }
}
