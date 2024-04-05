#' Calculate the average wind speed and direction for tracks
#' @inheritParams add_vleemo_observed_track
#' @export
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom RSQLite dbExecute dbListFields
#' @importFrom utils head tail
add_wind <- function(local, remote) {
  assert_that(
    inherits(local, "SQLiteConnection"),
    inherits(remote, "PostgreSQLConnection")
  )
  if (!"wind_speed" %in% dbListFields(local, "track_time")) {
    dbExecute(
      conn = local, statement = "ALTER TABLE track_time ADD wind_speed NUMERIC"
    )
  }
  if (!"wind_direction" %in% dbListFields(local, "track_time")) {
    dbExecute(
      conn = local,
      statement = "ALTER TABLE track_time ADD wind_direction NUMERIC"
    )
  }
  "SELECT t.id, t.start, t.duration, s.scheme
FROM track_time AS t
INNER JOIN scheme AS s ON t.scheme_id = s.id
INNER JOIN track_equal_time AS te ON t.id = te.track_id
WHERE t.wind_speed IS NULL" |>
    dbGetQuery(conn = local) -> to_do
  cli_progress_bar(
    "Calculating average wind speed and direction", total = nrow(to_do)
  )
  while (nrow(to_do) > 0) {
    "WITH cte AS (
  SELECT
    windspeed * cosd(90 - winddirectiontrue) AS dx,
    windspeed * sind(90 - winddirectiontrue) AS dy
  FROM %s.weather
  WHERE '%s' <= timestamp AND timestamp <= '%s'
)
SELECT
  sqrt(POWER(AVG(dx), 2) + POWER(AVG(dy), 2)) AS wind_speed,
  90 - atan2d(AVG(dy), AVG(dx)) AS wind_direction
FROM cte" |>
      sprintf(
        head(to_do$scheme, 1), as.POSIXct(head(to_do$start, 1)),
        as.POSIXct(head(to_do$start, 1) + head(to_do$duration, 1))
      ) |>
      dbGetQuery(conn = remote) -> speed_direction
    sprintf(
      "UPDATE track_time
SET wind_speed = %f, wind_direction = %f
WHERE id = %i",
      speed_direction$wind_speed, speed_direction$wind_direction,
      head(to_do$id, 1)
    ) |>
      dbExecute(conn = local)
    to_do <- tail(to_do, -1)
    cli_progress_update()
  }
  cli_progress_done()
}
