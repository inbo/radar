#' Import basic track information
#'
#' Select all tracks with manual observations.
#' The minimal duration is 1 minute.
#' The airspeed is between 5 and 30 m/s.
#' @param remote the connection to the remote database
#' @param local the connection to the local database
#' @param scheme the scheme to import
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom RPostgreSQL dbClearResult dbGetQuery dbRemoveTable dbSendQuery
#' dbWriteTable
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
    dbSendQuery(conn = local) |>
    dbClearResult()
  sprintf("
INSERT INTO species
SELECT DISTINCT f.id, t.common_name, 0 AS relevant
FROM %s AS t
LEFT JOIN species AS f ON t.common_name = f.common_name
WHERE f.id IS NULL
ORDER BY t.common_name",
    temp_table
  ) |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  "CREATE TABLE IF NOT EXISTS scheme
(id INTEGER PRIMARY KEY AUTOINCREMENT, scheme TEXT UNIQUE NOT NULL)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  sprintf("INSERT OR IGNORE INTO scheme (scheme) VALUES ('%s')", scheme) |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  sprintf("SELECT id FROM scheme WHERE scheme = '%s'", scheme) |>
    dbGetQuery(conn = local) -> scheme_id
  "CREATE TABLE IF NOT EXISTS track_time
(
  id INTEGER PRIMARY KEY, scheme_id INTEGER, species_id INTEGER,
  start INTEGER NOT NULL, duration NUMERIC NOT NULL
)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  sprintf(
    "INSERT INTO track_time
SELECT t.id, %i AS scheme_id, s.id AS species_id, t.start, t.duration
FROM %s AS t
LEFT JOIN species AS s ON t.common_name = s.common_name
LEFT JOIN track_time AS c ON t.id = c.id
WHERE c.start IS NULL", scheme_id$id, temp_table
  ) |>
    dbSendQuery(conn = local) |>
    dbClearResult()
}
