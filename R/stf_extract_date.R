#' Extract dates from opinions' text
#'
#' @param x Opinion's text
#'
#' @return Vector of dates
#' @export
#'
stf_extract_date <- function(x = NULL){

  d <- x %>%
    stringr::str_sub(-2000) %>%
    stringr::str_extract_all("Brasília.+?(?=(\\.|\n))") %>%
    unlist() %>%
    tail(1) %>%
    stringr::str_squish() %>%
    stringr::str_remove("º") %>%
    lubridate::parse_date_time(orders = "Brasília, %d de %b de %Y",
                               exact = TRUE, locale = "pt_BR.UTF-8")

  if (length(d) == 0){

    d <- x %>%
      stringr::str_squish() %>%
      stringr::str_extract_all("\\d{1,2}\\.\\d{1,2}\\.\\d{4}") %>%
      unlist() %>%
      tail(1) %>%
      lubridate::parse_date_time(orders="%d.%m%Y")
  } else {

    d <- d
  }

  return(d)

}
