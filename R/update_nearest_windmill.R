#' Calculate the distance to the nearest windmill
#' @inheritParams add_vleemo_bbox
#' @export
#' @importFrom assertthat assert_that is.count noNA
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom RSQLite dbClearResult dbGetQuery dbSendQuery
#' @importFrom utils head tail
update_nearest_windmill <- function(local, step = 1000) {
  assert_that(inherits(local, "SQLiteConnection"), is.count(step), noNA(step))
  "CREATE TABLE IF NOT EXISTS windmill_track_distance
(
  id INTEGER PRIMARY KEY, track_id INTEGER NOT NULL,
  windmill_id TEXT NOT NULL, distance NUMERIC NOT NULL,
  FOREIGN KEY(track_id) REFERENCES track_equal_time(id),
  FOREIGN KEY(windmill_id) REFERENCES windmill(id)
)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  "CREATE UNIQUE INDEX IF NOT EXISTS windmill_track_distance_idx
ON windmill_track_distance (track_id, windmill_id)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
"DELETE FROM windmill_track_distance WHERE id IN (
  SELECT wtd.id
  FROM windmill_track_distance AS wtd
  LEFT JOIN windmill AS w ON w.id = wtd.windmill_id
  WHERE w.id IS NULL
)" |>
  dbSendQuery(conn = local) |>
  dbClearResult()
  "SELECT tet.id
FROM track_equal_time AS tet
LEFT JOIN windmill_track_distance AS wtd ON wtd.track_id = tet.id
WHERE tet.part >= 0 AND wtd.distance IS NULL" |>
    dbGetQuery(conn = local) -> to_do
  cli_progress_bar(
    "Calculate nearest windmill", total = nrow(to_do)
  )
  while (nrow(to_do) > 0) {
    head(to_do$id, step) |>
      paste(collapse = ", ") |>
      sprintf(
        fmt = "WITH cte_raw AS (
  SELECT
    tet.id AS track_id, w.id AS windmill_id,
    SQRT(POWER(tetp.x - w.x, 2) + POWER(tetp.y - w.y, 2)) AS distance
  FROM track_equal_time AS tet
  INNER JOIN track_equal_time_point AS tetp ON tetp.equal_time_id = tet.id
  CROSS JOIN windmill AS w
  WHERE tet.id IN (%s)
),
cte_distance AS (
  SELECT track_id, windmill_id, distance
  FROM cte_raw
  GROUP BY track_id
  HAVING MIN(distance)
  ORDER BY distance
)
INSERT INTO windmill_track_distance
SELECT NULL AS id, track_id, windmill_id, distance
FROM cte_distance"
      ) |>
      dbSendQuery(conn = local) |>
      dbClearResult()
    to_do <- tail(to_do, -step)
    cli_progress_update(inc = step)
  }
  cli_progress_done()
}
