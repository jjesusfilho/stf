#' Download html with parties based on incidente
#'
#' @param incidente Incidente number
#' @param dir Directory where to download the htmls
#'
#' @return htmls
#' @export
#'
stf_download_parties <- function(incidente, dir = "."){

  pb <- progress::progress_bar$new(total = length(incidente))

  purrr::walk(incidente,purrr::possibly(~{

    pb$tick()

    url <- paste0("http://portal.stf.jus.br/processos/abaPartes.asp?incidente=",.x)

    arquivo <- file.path(dir,paste0(format(Sys.Date(), "date_%Y_%m_%d_incidente_"), .x, ".html"))

    httr::RETRY("GET",url,httr::write_disk(arquivo),
                httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36")

                )

  },NULL))

}
