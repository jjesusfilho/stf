#' Read precedentes downloaded with stf_download_cjsg
#'
#' @param files Vector of json files
#' @param dir  If you don't informe file, provide the dir.
#'
#' @return tibble
#' @export
#'
stf_read_cjsg <- function(files = NULL, dir = "."){

  if (is.null(files)){

    files <- list.files(dir, full.names = T, pattern = "json$")

  }


 purrr::map_dfr(files, purrr::possibly(~{

     .x |>
      jsonlite::fromJSON() |>
      purrr::pluck(\(x) x$result$hits$hits$`_source`)

 }, NULL))


}
