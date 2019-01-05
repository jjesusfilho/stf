#' Wraper around functions download_stf_collection and read_stf_collection.
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
get_stf_collection <- function(decision_type = NULL, years = NULL) {

 dir <- tempdir()
 on.exit(unlink(dir))

 download_stf_collection(decision_type=decision_type,years=years,dir=dir)
 read_stf_collection(dir=dir,classes=NULL,years=years)

}
