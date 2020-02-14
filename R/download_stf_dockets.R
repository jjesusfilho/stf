#' Downloads Brazilian Supreme Court lawsuits based on the proceding class and the docket number.
#'
#' @param class This is what in Brazil is called classe processual
#' @param docket_number Number of the lawsuit
#' @param abas c("partes","andamentos","informacoes","detalhes")
#' @param dir directory under which all subdirectories will
#'     be placed.
#'
#' @return htmls in nine folders corresponding to each one of the stf webpage tabs.
#' @export
#'
#' @examples
#' \dontrun{
#' download_stf_dockets(class = "HC", docket_number = "4050")
#' }
download_stf_dockets <- function(class = NULL,
                                 docket_number = NULL,
                                 abas = c("detalhes","partes","andamentos","informacoes"),
                                 dir = ".") {
  if (is.null(class) | is.null(docket_number)) {
    stop("You must provide both the class and the docket_number")
  }

  if (abas[[1]]!="detalhes"){

    stop("The first aba must be 'detalhes'")
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
    abas %>%
    file.path(dir, .)

  purrr::walk(diretorios, dir.create)

  incidente<-  purrr::map(urls,~{

    resposta <-  httr::RETRY("GET",.x)
    incidente <- resposta$url %>%
      stringr::str_extract("\\d+")
    arquivo <- paste0(dir, "/detalhes", format(Sys.Date(), "/date_%Y_%m_%d_"), incidente, ".html")
    writeBin(resposta$content,arquivo)
    incidente
  }) %>%
    unlist()

  #purrr::walk2(detalhes, arquivos, purrr::possibly(~ writeBin(.x$content, .y), NULL))


  bases<-purrr::map(abas[-1],~{

    sprintf("http://portal.stf.jus.br/processos/aba%s.asp?incidente=%s",stringr::str_to_title(.x),incidente)
  }) %>%
    purrr::set_names(paste0("url_",abas[-1]))




  purrr::walk2(bases, diretorios[-1], purrr::possibly(~ {

    cd <- crul::Async$new(urls <- .x)

    res <- cd$get()

    incidente <- stringr::str_extract(.x, "\\d+")

    arquivos <- paste0(.y, format(Sys.Date(), "/date_%Y_%m_%d_"), incidente, ".html")


    purrr::walk2(res, arquivos, purrr::possibly(~ writeBin(.x$content, .y), NULL))
  }, NULL))
}
