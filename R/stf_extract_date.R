#' Extract dates from opinions' text
#'
#' @param x Opinion's text
#'
#' @return Vector of dates
#' @export
#'
stf_extract_date <- function(x = NULL){

  purrr::map(x,purrr::possibly(~{

    d <- .x %>%
      stringr::str_sub(-2000) %>%
      stringr::str_remove_all("u2013 resid\u00eancia u2013") %>%
      stringr::str_extract_all("Bras\u00edlia.+?\\d{4}") %>%
      unlist() %>%
      tail(1) %>%
      stringr::str_remove_all("(Bras[\u00edi]lia|\\bde\\b|,)") %>%
      stringr::str_squish() %>%
      stringr::str_remove("\u00ba|\u00b0") %>%
      lubridate::parse_date_time(orders = "%d %b %Y",
                                 exact = TRUE, locale = "pt_BR.UTF-8") %>%
      as.character()

    if (length(d) == 0){

      d <- .x %>%
        stringr::str_sub(-2000) %>%
        stringr::str_squish() %>%
        stringr::str_remove("\u00ba|\u00b0") %>%
        stringr::str_extract_all("\\d{1,2}[\\.|/]\\d{1,2}[\\.|/]\\d{4}") %>%
        unlist() %>%
        tail(1) %>%
        stringr::str_replace_all("\\D"," ") %>%
        lubridate::parse_date_time(orders="%d %m %Y") %>%
        as.character()
    } else {

      d <- d
    }

    d

  },NA_character_)) %>%
    purrr::map(~vctrs::`%0%`(.x,NA_character_)) %>%
    unlist() %>%
    as.Date()
}
