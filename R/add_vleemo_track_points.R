#' @inheritParams equal_time_track
#' @inheritParams add_vleemoe_observed_track
#' @importFrom assertthat assert_that
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
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
  dbGetQuery(
    conn = local,
    "SELECT t.id, s.scheme, tt.scheme_id, tt.track_id
  FROM track_bbox AS t
  INNER JOIN track_time AS tt ON t.id = tt.id
  INNER JOIN scheme AS s ON tt.scheme_id = s.id
  LEFT JOIN track_equal_time AS c ON t.id = c.track_id
  WHERE c.rate IS NULL"
  ) -> track_id
  cli_progress_bar(
    name = "add missing vleemo track points", total = nrow(track_id)
  )
  while (nrow(track_id) > 0) {
    i <- sample(nrow(track_id), 1)
    sprintf(
      "SELECT
    id, ST_Transform(trajectory, 31370) AS track, trajectory_time AS time,
    trajectory_radarid AS radar
  FROM %s.track
  WHERE id = %i",
      track_id$scheme[i], track_id$track_id[i]
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
    if (nrow(raw_track) == 0) {
      data.frame(track_id = track_id$id[i], part = -1, rate = rate) |>
        dbAppendTable(conn = local, name = "track_equal_time")
      track_id <- track_id[-i, ]
      cli_progress_update()
      next
    }
    data.frame(
      track_id = track_id$id[i], part = unique(raw_track$part), rate = rate
    ) |>
      dbAppendTable(conn = local, name = "track_equal_time")
    sprintf(
      "SELECT id AS equal_time_id, part
  FROM track_equal_time
  WHERE track_id = %i AND rate = %f",
      track_id$id[i], rate
    ) |>
      dbGetQuery(conn = local) |>
      inner_join(raw_track, by = "part") |>
      select(-"part") |>
      dbAppendTable(conn = local, name = "track_equal_time_point")
    track_id <- track_id[-i, ]
    cli_progress_update()
  }
  cli_progress_done()
}
