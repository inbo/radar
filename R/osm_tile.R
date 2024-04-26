#' return OSM raster tiles for a given bounding box
#' @inheritParams osmdata::opq
#' @param extra_zoom the number of extra zoom levels
#' @param cache_path File location to cache maps
#' @export
#' @importFrom assertthat assert_that has_name is.number noNA
#' @importFrom curl curl_download handle_setheaders new_handle
#' @importFrom dplyr filter mutate transmute
#' @importFrom png readPNG
#' @importFrom purrr walk walk2
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tools R_user_dir
#' @importFrom utils file_test
osm_tile <- function(
  bbox = c(xmin = 4.26, ymin = 51.22, xmax = 4.50, ymax = 51.35),
  extra_zoom = 0, cache_path = R_user_dir("birdradar", which = "cache")
) {
  assert_that(
    inherits(bbox, c("bbox", "numeric")), has_name(bbox, "xmin"),
    has_name(bbox, "xmax"), has_name(bbox, "ymin"), has_name(bbox, "ymax"),
    -180 <= bbox["xmin"], bbox["xmax"] < 180, -90 <= bbox["ymin"],
    bbox["ymax"] < 90, noNA(bbox), is.number(extra_zoom), is.string(cache_path),
    noNA(cache_path)
  )
  which((bbox["xmax"] - bbox["xmin"]) > 360 / 2 ^ (0:19)) |>
    c(
      which((bbox["ymax"] - bbox["ymin"]) > 170.1012 / 2 ^ (0:19))
    ) |>
    min() -> zoom
  min(zoom + as.integer(extra_zoom), 19) |>
    max(0) -> zoom
  expand.grid(
    lon = seq(bbox["xmin"], bbox["xmax"], 360 * 2 ^ -zoom),
    lat = seq(bbox["ymin"], bbox["ymax"], 170.1012 * 2 ^ -zoom)
  ) |>
    mutate(
      lat_rad = .data$lat * pi / 180,
      tile_x = floor(2 ^ zoom * (.data$lon + 180) / 360),
      tile_y = floor(
        2 ^ (zoom - 1) *
          (1 - log(tan(.data$lat_rad) + 1 / cos(.data$lat_rad)) / pi)
      ),
      url = sprintf(
        "https://a.tile.openstreetmap.org/%.0f/%.0f/%.0f.png", zoom,
        .data$tile_x, .data$tile_y
      ),
      cache = str_replace(
        .data$url, "https://a.tile.openstreetmap.org", cache_path
      ),
      available = file_test("-f", .data$cache),
      xmin = .data$tile_x * 2 ^ -zoom * 360 - 180,
      xmax = (.data$tile_x + 1) * 2 ^ -zoom * 360 - 180,
      ymin = atan(
        sinh(pi - (.data$tile_y + 1) * 2 * pi * 2 ^ -zoom)
      ) * 180 / pi,
      ymax = atan(sinh(pi - .data$tile_y * 2 * pi * 2 ^ -zoom)) * 180 / pi
    ) -> tiles
  tiles |>
    filter(!.data$available) -> to_do
  dirname(to_do$cache) |>
    unique() |>
    walk(~dir.create(.x, showWarning = FALSE, recursive = TRUE))
  walk2(
    to_do$url, to_do$cache,
      ~new_handle() |>
        handle_setheaders(`User-Agent` = "birdradar") |>
        curl_download(url = .x, destfile = .y, quiet = TRUE, mode = "wb")
  )
  tiles |>
    transmute(
      .data$xmin, .data$xmax, .data$ymin, .data$ymax,
      raster = map(.data$cache, readPNG)
    )
}
