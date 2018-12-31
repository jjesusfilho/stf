#' Gets Brazilian Supreme Courts collection
#'
#' @param decision_type either "monocraticas", "presidente" or "colegiadas"
#' @param years vector of years
#'
#' @return tibble with metadata of all supreme court's decisions
#' @export
#'
#' @examples
#' \dontrun{
#' get_stf_colletion(decision_type="monocraticas",years=c(2017,2018))
#' }
get_stf_collection <- function(decision_type = NULL, years = NULL){


  if (!is.numeric(years)) {

    stop("years must be numeric")
  }

  urls <- switch(decision_type,
               colegiadas = sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/colegiadas/decisoes_colegiadas_geral_lista_%d.xlsx",years),
               monocraticas= sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/decisoes_monocraticas_lista_%d.xlsx",years),
               presidentes= sprintf("http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/decisoes_monocraticas_lista_presidente_%d.xlsx",years)

  )



  temp_dir<-tempdir()

  files<-tempfile(stringr::str_extract(urls,"decisoes_.+(?=\\.)"),fileext = ".xlsx")

  purrr::map2(urls,files,~httr::GET(.x,httr::write_disk(.y,overwrite = T)))


  decisoes<-purrr::map2_dfr(files,urls,~{
    df<- .x %>%
      readxl::read_excel(col_names = FALSE) %>%
      dplyr::filter(dplyr::row_number()>=stringr::str_which(X__1,"Classe"))

    n<-t(df[1,])

    df %>%
      purrr::set_names(n) %>%
      tail(-1) %>%
      tibble::add_column(file=.y)

  })

  decisoes<- decisoes %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(classe))

  base<-purrr::map_dfr(files,~{

    zip<-stringr::str_replace(.x,"xlsx$","zip")

    file.copy(from = .x, to = zip)


    unzip(zip,overwrite = T,exdir = temp_dir)

    x<-  xml2::read_xml(paste0(temp_dir,"/xl/worksheets/_rels/sheet1.xml.rels"))
    hyperlink<-xml2::xml_find_all(x, "//@Target") %>%
      xml2::xml_text()

    id <- xml2::xml_find_all(x, "//@Id") %>%
      xml2::xml_text()

    tibble::tibble(hyperlink=hyperlink,linha=id)
  },.id="id")
unlink(temp_dir)
decisoes %>%
    dplyr::bind_cols(base,.) %>%
    dplyr::filter(classe==classe) %>%
    dplyr::mutate(na=NULL,
                  na_1=NULL)
}
