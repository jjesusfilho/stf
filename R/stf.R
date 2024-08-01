#' \code{stf} package
#'
#' Downloads and organizes Brazilian Supreme Court's Decisions
#'
#'
"_PACKAGE"
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "classe",  "incidente","doc_url",
                           "assunto1", "assunto2", "assunto3", "data_protocolo", "numero_origem",
                           "orgao_origem", "origem", "procedencia"))
}
