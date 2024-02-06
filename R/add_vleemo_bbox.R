#' Add missing bounding box information for relevant tracks
#' @inheritParams add_vleemo_observed_track
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom RSQLite dbAppendTable dbClearResult dbGetQuery dbSendQuery
#' @export
add_vleemo_bbox <- function(local, remote) {
  assert_that(
    inherits(local, "SQLiteConnection"),
    inherits(remote, "PostgreSQLConnection")
  )
  "CREATE TABLE IF NOT EXISTS track_bbox
(
  id INTEGER PRIMARY KEY, scheme_id INTEGER, length NUMERIC NOT NULL,
  convex_hull NUMERIC NOT NULL, x_min NUMERIC NOT NULL, x_max NUMERIC NOT NULL,
  y_min NUMERIC NOT NULL, y_max NUMERIC NOT NULL, z_min NUMERIC NOT NULL,
  z_max NUMERIC NOT NULL
)" |>
  dbSendQuery(conn = local) |>
  dbClearResult()
  while (TRUE) {
    dbGetQuery(
      local,
      "WITH relevant AS (SELECT id FROM species WHERE relevant > 0)
  SELECT t.id, s.scheme, t.scheme_id
  FROM relevant AS r
  INNER JOIN track_time AS t ON r.id = t.species_id
  INNER JOIN scheme AS s ON t.scheme_id = s.id
  LEFT JOIN track_bbox AS b ON t.id = b.id AND t.scheme_id = b.scheme_id
  WHERE b.length IS NULL
  LIMIT 10"
    ) -> track_id
    if (nrow(track_id) == 0) {
      break
    }
    track_id <- track_id[track_id$scheme == head(track_id$scheme, 1), ]
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
    id, length, convex_hull, ST_Xmin(bbox) AS x_min, ST_Xmax(bbox) AS x_max,
    ST_Ymin(bbox) AS y_min, ST_Ymax(bbox) AS y_max,
    ST_Zmin(bbox) AS z_min, ST_Zmax(bbox) AS z_max
  FROM cte",
      head(track_id$scheme, 1), paste(track_id$id, collapse = ", ")
    ) |>
      dbGetQuery(conn = remote) -> track_bbox
    track_bbox$scheme_id <- track_id$scheme_id
    dbAppendTable(conn = local, name = "track_bbox", value = track_bbox)
  }
}
