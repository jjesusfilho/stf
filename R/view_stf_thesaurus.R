#' Displays STF's thesaurus.
#'
#' @keywords stf, thesaurus
#' @return This function has only side effect, it doesn't return anything.
#' @examples
#' \dontrun{
#' view_stf_thesaurus()
#' }
#' 
#' @export
view_stf_thesaurus <- function() {
  url <- "http://www.stf.jus.br/portal/jurisprudencia/pesquisarVocabularioJuridico.asp"
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(url)
  } else {
    utils::browseURL(url)
  }
}
