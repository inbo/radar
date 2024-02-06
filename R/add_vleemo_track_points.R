#' @inheritParams equal_time_track
#' @inheritParams add_vleemoe_observed_track
#' @importFrom assertthat assert_that
#' @importFrom dplyr transmute
#' @importFrom sf st_coordinates st_read
#' @importFrom rlang .data
#' @importFrom RSQLite dbAppendTable dbClearResult dbGetQuery dbSendQuery
#' @export
add_vleemo_track_points <- function(local, remote, rate = 2) {
  assert_that(
    inherits(local, "SQLiteConnection"),
    inherits(remote, "PostgreSQLConnection")
  )
  "CREATE TABLE IF NOT EXISTS track_equal_time
  (
    id INTEGER PRIMARY KEY AUTOINCREMENT, track_id INTEGER NOT NULL,
    part INTEGER NOT NULL, rate INTEGER NOT NULL CHECK (rate > 0),
    FOREIGN KEY(track_id) REFERENCES track_time(id)
  )" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  "CREATE UNIQUE INDEX IF NOT EXISTS track_equal_time_idx ON track_equal_time
  (track_id, part, rate)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  "CREATE TABLE IF NOT EXISTS track_equal_time_point
  (
    id INTEGER PRIMARY KEY AUTOINCREMENT, equal_time_id INTEGER NOT NULL,
    t NUMERIC NOT NULL, x NUMERIC NOT NULL, y NUMERIC NOT NULL,
    z NUMERIC NOT NULL, step_2d NUMERIC NOT NULL, step_3d NUMERIC NOT NULL,
    yaw NUMERIC NOT NULL, pitch NUMERIC NOT NULL, delta_yaw NUMERIC NOT NULL,
    delta_pitch NUMERIC NOT NULL,
    FOREIGN KEY(equal_time_id) REFERENCES track_equal_time(id)
  )" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  "CREATE UNIQUE INDEX IF NOT EXISTS track_equal_time_point_idx
  ON track_equal_time_point (equal_time_id, t)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  while (TRUE) {
    dbGetQuery(
      conn = local,
      "SELECT t.id, s.scheme, t.scheme_id
  FROM track_bbox AS t
  INNER JOIN scheme AS s ON t.scheme_id = s.id
  LEFT JOIN track_equal_time AS c ON
    t.scheme_id = c.scheme_id AND t.id = c.track_id
  WHERE c.rate IS NULL
  LIMIT 1"
    ) -> track_id
    if (nrow(track_id) == 0) {
      break
    }
    message(track_id$scheme, " ", track_id$id)
    sprintf(
      "SELECT
    id, ST_Transform(trajectory, 31370) AS track, trajectory_time AS time,
    trajectory_radarid AS radar
  FROM %s.track
  WHERE id = %i",
      track_id$scheme, track_id$id
    ) -> query
    tracks <- st_read(dsn = remote, query = query)
    strsplit(tracks$radar, ",") |>
      unlist() |>
      gsub(pattern = "(\\{|\\})", replacement = "") |>
      as.integer() -> radar
    assert_that(length(unique(radar)) == 1)
    strsplit(tracks$time, ",") |>
      unlist() |>
      gsub(pattern = "(\\{|\\})", replacement = "") |>
      as.numeric() -> time
    st_coordinates(tracks) |>
      as.data.frame() |>
      transmute(x = .data$X, y = .data$Y, z = .data$Z, t = time) |>
      equal_time_track(rate = rate) -> raw_track
    track_id |>
      transmute(track_id = .data$id, .data$scheme_id, rate = rate) |>
      dbAppendTable(conn = local, name = "track_equal_time")
    sprintf(
      "SELECT id FROM track_equal_time WHERE track_id = %i AND scheme_id = %i",
      track_id$id, track_id$scheme_id
    ) |>
      dbGetQuery(conn = local) -> equal_id
    raw_track |>
      mutate(equal_time_id = equal_id$id) |>
      dbAppendTable(conn = local, name = "track_equal_time_point")
  }
}
