#' \code{stf} package
#'
#' Downloads and organizes Brazilian Supreme Court's Decisions
#'
#'
#' @docType package
#' @name stf
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "classe",  "incidente","doc_url"))
}
