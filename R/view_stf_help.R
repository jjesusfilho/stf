#' Function stf_help_view
#'
#' This function displays STF's help page on R viewer pane.
#' @keywords stf, help
#' @return This function has only a side effect, it doesn't return any object or file.
#' @examples
#' \dontrun{
#' view_stf_help()
#' }

#' @export
view_stf_help <- function(){
  url<-"http://www.stf.jus.br/portal/cms/verTexto.asp?servico=jurisprudenciaPesquisaGeral&pagina=ajudaPesquisaJurisprudencia&popup=S"

  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(url)
  else
    utils::browseURL(url)

}


