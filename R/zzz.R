stfsql <- environment()

.onLoad <- function(...) {

  assign("conn", DBI::dbConnect(RSQLite::SQLite(), ":memory:"), envir = stfsql)
}

.onUnload <- function(...) {
  DBI::dbDisconnect(conn)
}
