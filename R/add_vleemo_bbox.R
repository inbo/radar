#' Add missing bounding box information for relevant tracks
#' @inheritParams add_vleemo_observed_track
#' @param step The number of tracks to handle in a single step.
#' @importFrom assertthat assert_that is.count is.string noNA
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom RSQLite dbAppendTable dbClearResult dbGetQuery dbSendQuery
#' @importFrom utils head
#' @export
add_vleemo_bbox <- function(local, remote, step = 1000) {
  assert_that(
    inherits(local, "SQLiteConnection"), is.count(step), noNA(step),
    inherits(remote, "PostgreSQLConnection")
  )
  "CREATE TABLE IF NOT EXISTS track_bbox
(
  id INTEGER PRIMARY KEY, length NUMERIC NOT NULL, convex_hull NUMERIC NOT NULL,
  x_min NUMERIC NOT NULL, x_max NUMERIC NOT NULL, y_min NUMERIC NOT NULL,
  y_max NUMERIC NOT NULL, z_min NUMERIC NOT NULL, z_max NUMERIC NOT NULL,
  FOREIGN KEY(id) REFERENCES track_time(id)
)" |>
  dbSendQuery(conn = local) |>
  dbClearResult()
  dbGetQuery(
    local,
    "WITH relevant AS (SELECT id FROM species WHERE relevant > 0)
    SELECT t.id, t.track_id, s.scheme, t.scheme_id
    FROM relevant AS r
    INNER JOIN track_time AS t ON r.id = t.species_id
    INNER JOIN scheme AS s ON t.scheme_id = s.id
    LEFT JOIN track_bbox AS b ON t.id = b.id
    WHERE b.length IS NULL"
  ) -> track_id
  cli_progress_bar(name = "Add missing track bbox", total = nrow(track_id))
  while (nrow(track_id) > 0) {
    which(track_id$scheme == head(track_id$scheme, 1)) |>
      head(step) -> this_track
    sprintf(
      "WITH cte_31370 AS (
    SELECT id, ST_Transform(trajectory, 31370) AS track
    FROM %s.track
    WHERE id IN (%s)
  ),
  cte AS (
    SELECT
      id, ST_Length(track) AS length,
      ST_Area(ST_ConvexHull(track)) AS convex_hull,
      Box3D(track) AS bbox
    FROM cte_31370
  )
  SELECT
    id AS track_id, length, convex_hull, ST_Xmin(bbox) AS x_min,
    ST_Xmax(bbox) AS x_max, ST_Ymin(bbox) AS y_min, ST_Ymax(bbox) AS y_max,
    ST_Zmin(bbox) AS z_min, ST_Zmax(bbox) AS z_max
  FROM cte",
      head(track_id$scheme, 1),
      paste(track_id$track_id[this_track], collapse = ", ")
    ) |>
      dbGetQuery(conn = remote) |>
      inner_join(
        track_id[this_track, ] |>
          select("id", "track_id"),
        by = "track_id"
      ) |>
      select(-"track_id") |>
      dbAppendTable(conn = local, name = "track_bbox")
    cli_progress_update(inc = step)
    track_id <- track_id[-this_track, ]
  }
  cli_progress_done()
}
