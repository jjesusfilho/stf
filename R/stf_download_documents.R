#' Downloads documents from table created
#'     with stf_read_document_table
#'
#' @param url Document's url
#' @param dir Dir
#'
#' @return pdfs
#' @export
stf_download_documents <- function(url, dir = "."){

  pb <- progress::progress_bar$new(total = length(url))



  purrr::walk(url,purrr::possibly(~{

    pb$tick()

    doc_id <- stringr::str_extract(.x,"(?<=docID=)\\d+")

    incidente <- stringr::str_extract(.x, "(?<=prcID=)\\d+")

    arquivo <- file.path(dir, paste0("stf_docid_",doc_id,"_incidente_", incidente, ".pdf"))

   httr::GET(.x, httr::write_disk(arquivo))


  },NULL))

}
