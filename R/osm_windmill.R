#' Get windmill locations from OpenStreetMap
#'
#' Download the windmill information in the bounding box.
#' Store the location, height and operator in the local database.
#' @inheritParams add_vleemo_observed_track
#' @inheritParams osmdata::opq
#' @export
#' @importFrom RSQLite dbWriteTable
#' @importFrom dplyr bind_rows
#' @importFrom sf st_centroid st_coordinates st_distance st_transform
osm_windmill <- function(
  local, bbox = c(xmin = 4.26, ymin = 51.22, xmax = 4.50, ymax = 51.35)
) {
  stopifnot(requireNamespace("osmdata", quietly = TRUE))
  assert_that(inherits(local, "SQLiteConnection"))
  osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = "generator:source", value = "wind") |>
    osmdata::osmdata_sf() -> windmill
  windmill$osm_polygons |>
    st_transform(crs = 31370) |>
    st_centroid() -> windmill_poly
  windmill$osm_points |>
    st_transform(crs = 31370) -> windmill_point
  st_distance(windmill_point, windmill_poly) |>
    apply(1, min) -> min_dist
  windmill_point |>
    filter(min_dist >= 20) |>
    bind_rows(windmill_poly) -> windmill
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
