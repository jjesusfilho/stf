#' Gets the pdfs with the panel's opinion text  according to the metadata
#'     obtained by the stf_precedent_metadata function.
#'
#' @param df data frame with at least two columns, one with the url,
#'    and the other, named electronico, with a logical vector informing whether the pdf is
#'    a text(TRUE) or an image(FALSE).
#'
#' @keywords stf, precedents, opinion, supreme court
#'
#' @return vector with the whole content of the opinions.
#'
#' @export
get_stf_opinion <- function(df) {
  stopifnot(is.logical(df$eletronico))
  diretorio <- getwd()
  setwd(tempdir())
  tmp_file <- tempfile(pattern = "inteiro", fileext = ".pdf")
  on.exit(setwd(diretorio), unlink(tmp_file))
  file <- purrr::map2(df$url_inteiro_teor, df$eletronico, purrr::possibly(~{
    if (.y == TRUE) {
      pdftools::pdf_text(.x)
    } else {
      httr::GET(.x,httr::write_disk(path=tmp_file,overwrite=TRUE))
      files <- pdftools::pdf_convert(tmp_file,dpi=400)
      files %>%
        purrr::map(~{
          .x %>%
            tesseract::ocr(engine=tesseract::tesseract("por"))
        })

    }
  },NA_character_,quiet=FALSE))

  file %>% purrr::map_chr(~stringr::str_c(.x,sep="\n",collapse=""))
}




