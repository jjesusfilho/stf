#' classifies appeals from stf based on decision summary
#'
#' @param x strings vector
#'
#' @return tibble
#' @export
#'
stf_classify_appeal <- function (x)
{
  x <- x  %>% tolower() %>% stringi::stri_trans_general("latin-ascii") %>%
    stringi::stri_replace_all_regex("vencidos?\\X{60}","")


  dplyr::case_when(stringi::stri_detect_regex(x, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~
                     "duvida", stringi::stri_detect_regex(x, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~
                     "duvida", stringi::stri_detect_regex(x, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~
                     "duvida", stringi::stri_detect_regex(x, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~
                     "duvida", stringi::stri_detect_regex(x, "parcial\\w*\\sprovi\\w+") ~
                     "parcial", stringi::stri_detect_regex(x, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~
                     "n\u00e3o conhecido", stringi::stri_detect_regex(x, "desconh\\w+") ~
                     "desconhecido", stringi::stri_detect_regex(x, "nao\\s+conhec\\w+") ~
                     "desconhecido", stringi::stri_detect_regex(x, "(desprov\\w+|improv\\w+)") ~
                     "improvido", stringi::stri_detect_regex(x, "(nao|nega\\w+|negou|negado|negar)\\s+provi\\X*") ~
                     "improvido", stringi::stri_detect_regex(x, "provi\\w+") ~
                     "provido", stringi::stri_detect_regex(x, "mantiveram") ~
                     "improvido", stringi::stri_detect_regex(x, "acolh\\w+") ~
                     "provido", stringi::stri_detect_regex(x, "(deu|deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)") ~
                     "provido", stringi::stri_detect_regex(x, "(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)") ~
                     "improvido", stringi::stri_detect_regex(x, "(homolog|desistencia)") ~
                     "desist\u00eancia",
                   stringi::stri_detect_regex(x, "(anular\\w*|nulo|nula|nulidade)") ~
                     "anulado",
                   stringi::stri_detect_regex(x, "(rejeit)") ~
                     "embargos rejeitados",
                   stringi::stri_detect_regex(x, "\\bprocedente") ~
                     "procedente",
                   stringi::stri_detect_regex(x, "improcedente") ~
                     "improcedente",
                   stringi::stri_detect_regex(x, "(improvimento|improvido)") ~
                     "improvido",
                   stringi::stri_detect_regex(x, "(\\bprovimento|\\bprovido)") ~
                     "provido",
                   stringi::stri_detect_regex(x, "(indefiro|indeferido|indeferiu|indefere)") ~
                     "indeferido",
                   stringi::stri_detect_regex(x, "(\\bdeferido|\\bdefiro|\\bdeferiu|\\bdefere)") ~
                     "deferido",
                   stringi::stri_detect_regex(x, "rejeito os embargos") ~
                     "embargos rejeitados",
                   stringi::stri_detect_regex(x, "acolheu os embargos") ~
                     "embargos acolhidos",
                   stringi::stri_detect_regex(x,"(?i)extin..o") ~ "extinto",
                   stringi::stri_detect_regex(x, "diligencia") ~
                     "convers\u00e3o em dilig\u00eancia",
                   stringi::stri_detect_regex(x,"(prej|extin)") ~ "prejudicado/extinto",
                   stringi::stri_detect_regex(x,"converteu") ~ "convers\u00e3o de classe",

                   TRUE ~ NA_character_)
}
