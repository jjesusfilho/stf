#' Classify respondents in Complaints (Reclamacao por descumprimento)
#'
#' @param partes tibble read by read_stf_parties function.
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @return Same tibble with two new columns: instancia e segmento
#' @export
#'
#' @examples
#' \dontrun{
#' partes <- classify_respondent(partes)
#' }
classify_respondent <- function(partes){

  df <- df %>%
    dplyr::group_by_at(dplyr::vars(-.data$parte_nome)) %>%  # group by everything other than the value column.
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%  # build group index
    tidyr::spread(key=.data$parte, value=.data$parte_nome) %>%    # spread
    dplyr::select(-.data$row_id)

  ## Reclamados

reclamado <-  df %>%
    dplyr::select(.data$reclamado) %>%
    na.omit() %>%
    dplyr::count(.data$reclamado) %>%
    dplyr::mutate(instancia = dplyr::case_when(
      stringr::str_detect(.data$reclamado,"(?i)minist.rio") ~ "outros",
      stringr::str_detect(.data$reclamado,"(?i)conselho superior") ~ "segunda",
      stringr::str_detect(.data$reclamado,"(?i)superior") ~ "superior",
      stringr::str_detect(.data$reclamado,"(?i)(tribunal regional|trt|tre|trf|tribunal de justi.a|c.mara|se..o|turma|col.gio|\\tj)") ~ "segunda",
      stringr::str_detect(.data$reclamado,"(?i)(juiz|ju.za|juizado)") ~ "primeira",
      TRUE ~ "outros"
    )) %>%

    dplyr::mutate(segmento = dplyr::case_when(
      stringr::str_detect(.data$reclamado,"(?i)minit.rio") ~ "outros",
      stringr::str_detect(.data$reclamado,"(?i)(trabalho|trt|tst)") ~ "trabalho",
      stringr::str_detect(.data$reclamado,"(?i)(eleitoral|tre|tse)") ~ "eleitoral",
      stringr::str_detect(.data$reclamado,"(?i)(federal|se.ao|trf|regi.o)") ~ "federal",
      stringr::str_detect(.data$reclamado,"(?i)superior tribunal de justi.a") ~ "stj",
      stringr::str_detect(.data$reclamado,"(?i)(turma|col.gio|tribunal de justi.a|\\btj|vara|juiz|ju.za|vara|comarca)") ~ "estadual",

      TRUE ~ NA_character_
    ))

df <- df %>%
  dplyr::select(.data$incidente,.data$reclamado) %>%
  na.omit() %>%
  dplyr::left_join(reclamado[c("reclamado","instancia","segmento")], by = "reclamado") %>%
  na.omit()
}
