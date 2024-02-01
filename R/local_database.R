#' Connect to a local SQLite database
#' Connects to the `radar.sqlite` database
#' @param path the path of the database
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom fs is_dir path
#' @importFrom RSQLite dbConnect SQLite
local_database <- function(path = ".") {
  assert_that(is.string(path), noNA(path), is_dir(path))
  dbConnect(drv = SQLite(), path(path, "radar.sqlite"))
}
