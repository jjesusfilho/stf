#' Reads pdf files
#'
#' @param file of paths to the pdf files to be read.
#'
#' @return a tibble with tree columns: incidente, texto, and doc_id.
#' @export
#'
#' @examples
#' \dontrun{
#' read_stf_pdf(file="")
#' }
read_stf_pdf <- function(file=NULL){

  lista<-vector("list", length(file))

  for (i in seq_along(file))  {
    lista[[i]]<-pdftools::pdf_text(file[i])
  }

  texto <-  purrr::map_chr(lista,~paste(.x,collapse=""))

    doc_id <- stringr::str_extract(file,"(?<=docid_)\\d+")

    incidente <- stringr::str_extract(file,"(?<=incidente_)\\d+")

    tibble::tibble(incidente=incidente,texto=texto,doc_id=doc_id)

}
