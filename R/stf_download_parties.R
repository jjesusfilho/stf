#' Download html with parties based on incidente
#'
#' @param incidente Incidente number
#' @param dir Directory where to download the htmls
#'
#' @return htmls
#' @export
#'
stf_download_parties <- function(incidente, dir = "."){



  purrr::walk(incidente,purrr::possibly(purrrogress::with_progress(~{

    url <- paste0("http://portal.stf.jus.br/processos/abaPartes.asp?incidente=",.x)

    arquivo <- paste0(dir, "/partes", format(Sys.Date(), "/date_%Y_%m_%d_incidente_"), .x, ".html")

    httr::RETRY("GET",url,httr::write_disk(arquivo))

  }),NULL))

}
