#' Import basic track information
#'
#' Select all tracks with manual observations.
#' The minimal duration is 1 minute.
#' The airspeed is between 5 and 30 m/s.
#' @param remote the connection to the remote database
#' @param local the connection to the local database
#' @param scheme the scheme to import
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom RSQLite dbExecute dbGetQuery dbRemoveTable dbWriteTable
#' @export
add_vleemo_observed_track <- function(local, remote, scheme = "m202206") {
  assert_that(
    inherits(local, "SQLiteConnection"), is.string(scheme), noNA(scheme),
    inherits(remote, "PostgreSQLConnection")
  )
  temp_table <- paste0("temp_track_", sample(.Machine$integer.max, 1))
  on.exit(dbRemoveTable(conn = local, name = temp_table), add = TRUE)
  sprintf(
    "SELECT DISTINCT
  t.id, s.common_name, t.timestamp_start AS start,
  DATE_PART('hour', t.timestamp_end - t.timestamp_start) * 3600 +
    DATE_PART('minute', t.timestamp_end - t.timestamp_start) * 60 +
    DATE_PART('second', t.timestamp_end - t.timestamp_start) AS duration
FROM %1$s.observation AS o
INNER JOIN %1$s.track AS t ON o.track_id = t.id
INNER JOIN config.species AS s ON o.species_id = s.id
WHERE
  t.timestamp_end - t.timestamp_start > '00:01:00' AND
  5 <= t.airspeed AND t.airspeed <= 30", scheme) |>
    dbGetQuery(conn = remote) |>
    dbWriteTable(conn = local, name = temp_table)
  "CREATE TABLE IF NOT EXISTS species
(
  id INTEGER PRIMARY KEY AUTOINCREMENT, common_name TEXT UNIQUE NOT NULL,
  relevant INTEGER
)" |>
    dbExecute(conn = local)
  sprintf("
INSERT INTO species
SELECT DISTINCT f.id, t.common_name, 0 AS relevant
FROM %s AS t
LEFT JOIN species AS f ON t.common_name = f.common_name
WHERE f.id IS NULL
ORDER BY t.common_name",
    temp_table
  ) |>
    dbExecute(conn = local)
  "CREATE TABLE IF NOT EXISTS scheme
(id INTEGER PRIMARY KEY AUTOINCREMENT, scheme TEXT UNIQUE NOT NULL)" |>
    dbExecute(conn = local)
  sprintf("INSERT OR IGNORE INTO scheme (scheme) VALUES ('%s')", scheme) |>
    dbExecute(conn = local)
  sprintf("SELECT id FROM scheme WHERE scheme = '%s'", scheme) |>
    dbGetQuery(conn = local) -> scheme_id
  "CREATE TABLE IF NOT EXISTS track_time
(
  id INTEGER PRIMARY KEY AUTOINCREMENT, track_id INTEGER NOT NULL,
  scheme_id INTEGER, species_id INTEGER, start INTEGER NOT NULL,
  duration NUMERIC NOT NULL, wind_speed NUMERIC, wind_direction NUMERIC
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  FOREIGN KEY(species_id) REFERENCES species(id)
)" |>
  dbExecute(conn = local)
  sprintf(
    "INSERT INTO track_time
SELECT
  NULL AS id, t.id AS track_id, %i AS scheme_id, s.id AS species_id, t.start,
  t.duration, NULL AS wind_speed, NULL AS wind_direction
FROM %s AS t
LEFT JOIN species AS s ON t.common_name = s.common_name
LEFT JOIN track_time AS c ON t.id = c.id
WHERE c.start IS NULL", scheme_id$id, temp_table
  ) |>
    dbExecute(conn = local)
}
