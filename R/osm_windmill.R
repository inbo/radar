#' Get windmill locations from OpenStreetMap
#'
#' Download the windmill information in the bounding box.
#' Store the location, height and operator in the local database.
#' @inheritParams add_vleemo_observed_track
#' @inheritParams osmdata::opq
#' @export
#' @importFrom RSQLite dbWriteTable
#' @importFrom sf st_coordinates st_transform
osm_windmill <- function(
  local, bbox = c(xmin = 4.26, ymin = 51.22, xmax = 4.50, ymax = 51.35)
) {
  stopifnot(requireNamespace("osmdata", quietly = TRUE))
  assert_that(inherits(local, "SQLiteConnection"))
  osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = "generator:source", value = "wind") |>
    osmdata::osmdata_sf() -> windmill
  windmill$osm_points |>
    st_transform(crs = 31370) -> windmill
  st_coordinates(windmill) |>
    `colnames<-`(c("x", "y")) |>
    as.data.frame() |>
    cbind(
      id = windmill$osm_id,
      height = as.numeric(windmill$height),
      operator = windmill$operator
    ) |>
    dbWriteTable(conn = local, name = "windmill", overwrite = TRUE)
}
