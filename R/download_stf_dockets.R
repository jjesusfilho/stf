#' Downloads Brazilian Supreme Court lawsuits based on the proceding class and the docket number.
#'
#' @param class This is what in Brazil is called classe processual
#' @param docket_number Number of the lawsuit
#'
#' @return htmls in nine folders corresponding to each one of the stf webpage tabs.
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_dockets(class = "HC", docket_number = "4050")
#' }
download_stf_dockets <- function(class = NULL,
                                 docket_number = NULL) {
  if (is.null(class) | is.null(docket_number)) {
    stop("You must provide both the class and the docket_number")
  }

  ##

  urls <-
    paste0(
      "http://portal.stf.jus.br/processos/listarProcessos.asp?classe=",
      class,
      "&numeroProcesso=",
      docket_number
    )



  diretorios <-
    c(
      "partes",
      "andamentos",
      "informacoes",
      "deslocamentos",
      "peticoes",
      "decisoes",
      "recursos",
      "pautas"
    )

  purrr::walk(diretorios, dir.create)

  cd <- crul::Async$new(urls <- urls)

  detalhes <- cd$get()

  incidente <- purrr::map_chr(detalhes, ~ .x$url) %>%
    stringr::str_extract("\\d+")

  dir.create("detalhes")
  arquivos <- paste0("detalhes", format(Sys.Date(), "/date_%Y_%m_%d_"), incidente, ".html")


  purrr::walk2(detalhes, arquivos, purrr::possibly(~ writeBin(.x$content, .y), NULL))


  bases <- list(
    url_partes = sprintf(
      "http://portal.stf.jus.br/processos/abaPartes.asp?incidente=%s",
      incidente
    ),
    url_andamentos = sprintf(
      "http://portal.stf.jus.br/processos/abaAndamentos.asp?incidente=%s",
      incidente
    ),
    url_informacoes = sprintf(
      "http://portal.stf.jus.br/processos/abaInformacoes.asp?incidente=%s",
      incidente
    ),
    url_deslocamentos = sprintf(
      "http://portal.stf.jus.br/processos/abaDeslocamentos.asp?incidente=%s",
      incidente
    ),
    url_peticoes = sprintf(
      "http://portal.stf.jus.br/processos/abaPeticoes.asp?incidente=%s",
      incidente
    ),
    url_decisoes = sprintf(
      "http://portal.stf.jus.br/processos/abaDecisoes.asp?incidente=%s",
      incidente
    ),
    url_recursos = sprintf(
      "http://portal.stf.jus.br/processos/abaRecursos.asp?incidente=%s",
      incidente
    ),
    url_pautas = sprintf(
      "http://portal.stf.jus.br/processos/abaPautas.asp?incidente=%s",
      incidente
    )
  )


  purrr::walk2(bases, diretorios, purrr::possibly(~ {
    cd <- crul::Async$new(urls <- .x)

    res <- cd$get()

    incidente <- stringr::str_extract(.x, "\\d+")

    arquivos <- paste0(.y, format(Sys.Date(), "/date_%Y_%m_%d_"), incidente, ".html")


    purrr::walk2(res, arquivos, purrr::possibly(~ writeBin(.x$content, .y), NULL))
  }, NULL))
}
