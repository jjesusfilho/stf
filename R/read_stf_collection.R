#' Read STF collection
#'
#' @param dir directory where to find the xlsx files.
#' @param action proceding (classe)
#' @param year numeric vector of years to read.
#' @return a tibble with all columns read
#' @export
#'
#' @examples
#' \dontrun{
#' read_stf_collection(action=NULL,year=NULL)
#' }
read_stf_collection <- function(dir=".",action,year) {

files<-list.files(dir,full.names=TRUE, pattern = ".xlsx")

decisoes<-purrr::map_dfr(files,~{

  df<- .x %>%
    readxl::read_excel(col_names = FALSE) %>%
    dplyr::filter(dplyr::row_number()>=stringr::str_which(X__1,"Classe"))

  n<-t(df[1,])

  df %>%
    purrr::set_names(n) %>%
    tail(-1) %>%
    tibble::add_column(file=.x)

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

  incidente <- stringr::str_extract(hyperlink,"\\d{3,}")

  tibble::tibble(hyperlink=hyperlink,incidente)

},.id="id")

decisoes %>%
  dplyr::bind_cols(base,.) %>%
  dplyr::filter(classe==action) %>%
  dplyr::filter(year==year) %>%
  dplyr::select(!dplyr::starts_with("na"))

}
