#' Downloads lawsuit
#'
#' @param incidentes Vector of incidentes
#' @param dir Where do save the json files
#'
#' @return json
#' @export
#'
stf_download_lawsuit <- function(incidentes, dir = "."){

  uri <- "https://sistemas.stf.jus.br/peticionamento/api/processo/"

  purrr::walk(incidentes, purrr::possibly(~{

    arquivo <- file.path(dir, paste0("stf_lawsuit_",.x, ".json"))


    url <- paste0("https://sistemas.stf.jus.br/peticionamento/api/processo/",.x)


    jsonlite::fromJSON(url) |>
      jsonlite::write_json(path = arquivo)

  },NULL))

}
