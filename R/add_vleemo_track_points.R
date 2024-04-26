#' Import raw tracks and convert them to equal time spaced tracks
#' @inheritParams equal_time_track
#' @inheritParams add_vleemo_observed_track
#' @importFrom assertthat assert_that is.count noNA
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom dplyr transmute
#' @importFrom sf st_coordinates st_read
#' @importFrom rlang .data
#' @importFrom RSQLite dbAppendTable dbExecute dbGetQuery
#' @export
add_vleemo_track_points <- function(local, remote) {
  assert_that(
    inherits(local, "SQLiteConnection"),
    inherits(remote, "PostgreSQLConnection")
  )
  "CREATE TABLE IF NOT EXISTS track_equal_time
  (
    id INTEGER PRIMARY KEY AUTOINCREMENT, track_id INTEGER NOT NULL,
    part INTEGER NOT NULL, FOREIGN KEY(track_id) REFERENCES track_time(id)
  )" |>
    dbExecute(conn = local)
  "DROP INDEX IF EXISTS track_equal_time_idx" |>
    dbExecute(conn = local)
  on.exit(
    "CREATE UNIQUE INDEX IF NOT EXISTS track_equal_time_idx ON track_equal_time
    (track_id, part)" |>
      dbExecute(conn = local),
    add = TRUE
  )
  "CREATE TABLE IF NOT EXISTS track_equal_time_point
  (
    id INTEGER PRIMARY KEY AUTOINCREMENT, equal_time_id INTEGER NOT NULL,
    t NUMERIC NOT NULL, x NUMERIC NOT NULL, y NUMERIC NOT NULL,
    z NUMERIC NOT NULL,
    FOREIGN KEY(equal_time_id) REFERENCES track_equal_time(id)
  )" |>
    dbExecute(conn = local)
  "DROP INDEX IF EXISTS track_equal_time_point_idx" |>
    dbExecute(conn = local)
  on.exit(
    "CREATE UNIQUE INDEX IF NOT EXISTS track_equal_time_point_idx
    ON track_equal_time_point (equal_time_id, t)" |>
      dbExecute(conn = local),
    add = TRUE
  )
  "SELECT t.id, s.scheme, tt.scheme_id, tt.track_id
FROM track_bbox AS t
INNER JOIN track_time AS tt ON t.id = tt.id
INNER JOIN scheme AS s ON tt.scheme_id = s.id
INNER JOIN species AS sp ON tt.species_id = sp.id
LEFT JOIN track_equal_time AS c ON t.id = c.track_id
WHERE
  c.track_id IS NULL AND t.convex_hull > t.length AND t.z_max > t.z_min + 10
ORDER BY sp.relevant DESC, t.z_max, t.z_min" |>
    dbGetQuery(conn = local) -> track_id
  cli_progress_bar(
    name = "add missing vleemo track points", total = nrow(track_id)
  )
  while (nrow(track_id) > 0) {
    sprintf(
      "SELECT
  id, ST_Transform(trajectory, 31370) AS track, trajectory_time AS time
FROM %s.track
WHERE id = %i",
      head(track_id$scheme, 1), head(track_id$track_id, 1)
    ) -> query
    tracks <- st_read(dsn = remote, query = query)
    strsplit(tracks$time, ",") |>
      unlist() |>
      gsub(pattern = "(\\{|\\})", replacement = "") |>
      as.numeric() -> time
    st_coordinates(tracks) |>
      as.data.frame() |>
      transmute(x = .data$X, y = .data$Y, z = .data$Z, t = time) |>
      equal_time_track() -> raw_track
    if (nrow(raw_track) == 0) {
      data.frame(track_id = head(track_id$id, 1), part = -1) |>
        dbAppendTable(conn = local, name = "track_equal_time")
      track_id <- tail(track_id, -1)
      cli_progress_update()
      next
    }
    data.frame(
      track_id = head(track_id$id, 1), part = unique(raw_track$part)
    ) |>
      dbAppendTable(conn = local, name = "track_equal_time")
    sprintf(
      "SELECT id AS equal_time_id, part
  FROM track_equal_time
  WHERE track_id = %i",
      head(track_id$id, 1)
    ) |>
      dbGetQuery(conn = local) |>
      inner_join(raw_track, by = "part") |>
      select(-"part") |>
      dbAppendTable(conn = local, name = "track_equal_time_point")
    track_id <- tail(track_id, -1)
    cli_progress_update()
  }
  cli_progress_done()
}
