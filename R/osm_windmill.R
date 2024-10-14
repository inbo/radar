#' Get wind turbine locations from OpenStreetMap
#'
#' Download the wind turbine information in the bounding box.
#' Store the location, height, operator and power in the local database.
#' @inheritParams add_vleemo_observed_track
#' @inheritParams osmdata::opq
#' @param min_power Keep only wind turbines with at least this amount of installed
#' power.
#' Keeps all turbines without power information on OpenStreetMap.
#' The power is expressed in MW.
#' Defaults to 1 MW.
#' @export
#' @importFrom RSQLite dbWriteTable
#' @importFrom dplyr bind_rows
#' @importFrom sf st_centroid st_coordinates st_distance st_transform
osm_windmill <- function(
  local, bbox = c(xmin = 4.17, ymin = 51.22, xmax = 4.50, ymax = 51.40),
  min_power = 1
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
  str_replace(windmill$`generator:output:electricity`, ",", ".") |>
    str_remove(" *[mkM][wW]") |>
    as.numeric() -> power
  ifelse(
    str_detect(windmill$`generator:output:electricity`, "[mM]"), 1,
    ifelse(str_detect(windmill$`generator:output:electricity`, "k"), 1e-3, NA)
  ) * power -> power
  st_coordinates(windmill) |>
    `colnames<-`(c("x", "y")) |>
    as.data.frame() |>
    cbind(
      id = windmill$osm_id,
      height = as.numeric(windmill$height),
      operator = windmill$operator,
      power = power
    ) |>
    filter(is.na(.data$power) | .data$power >= min_power) |>
    dbWriteTable(conn = local, name = "windmill", overwrite = TRUE)
}
