#' Read STF collection
#'
#' @param dir directory where to find the xlsx files.
#' @param action proceding (classe)
#' @param year numeric vector of years to read.
#' @importFrom rlang .data
#' @return a tibble with all columns read
#' @export
#'
#' @examples
#' \dontrun{
#' read_stf_collection(action=NULL,year=NULL)
#' }
read_stf_collection1<-function (dir = ".", classes=NULL, years=NULL,plan="sequential")
{
  future::plan(plan)
  anos<-paste0(years,collapse="|") %>%
    paste0("(",.,")")
  files <- list.files(dir, full.names = TRUE, pattern = ".xlsx") %>%
    stringr::str_subset(anos)

  decisoes <- furrr::future_map_dfr(files, ~{
    df <- .x %>% readxl::read_excel(col_names = FALSE) %>%
      dplyr::filter(dplyr::row_number() >= stringr::str_which(X__1,
                                                              "Classe"))
    n <- t(df[1, ])
    df %>% purrr::set_names(n) %>%
      tail(-1)
  })

  decisoes <- decisoes %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(classe))

  base <- furrr::future_map_dfr(files, ~{
    temp_dir <- tempdir()
    zip <- stringr::str_replace(.x, "xlsx$", "zip")
    file.copy(from = .x, to = zip)
    unzip(zip, overwrite = T, exdir = temp_dir)

    x <- xml2::read_xml(paste0(temp_dir, "/xl/worksheets/_rels/sheet1.xml.rels"))


    hyperlink <- xml2::xml_find_all(x, "//@Target") %>%
      xml2::xml_text()

    incidente <- stringr::str_extract(hyperlink, "\\d{3,}")
    unlink(temp_dir)
    tibble::tibble(hyperlink = hyperlink, incidente)
  }, .id = "id")

  if (nrow(base)==nrow(decisoes)) {

  decisoes <- decisoes %>%
    dplyr::bind_cols(base, .)
  }

  if (!is.null(classes)) {
    decisoes <- decisoes %>%
      dplyr::filter(classe %in% classes)
  }

  decisoes %>%
    dplyr::select(-dplyr::starts_with("na")) %>%
    dplyr::mutate(data_autuacao = janitor::excel_numeric_to_date(as.numeric(.data$data_autuacao)),
                  data_andamento = janitor::excel_numeric_to_date(as.numeric(.data$data_andamento))) %>%
    dplyr::mutate(ano_andamento=lubridate::year(data_andamento))
}




