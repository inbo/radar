#' Calculate the summary of the available tracks
#'
#' The function calculates the number of tracks per combination of scheme,
#' classification and species.
#' @inheritParams add_vleemo_observed_track
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter left_join mutate select transmute
#' @importFrom RSQLite dbAppendTable dbExecute dbGetQuery
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
add_available_tracks <- function(local, remote) {
  assert_that(
    inherits(local, "SQLiteConnection"),
    inherits(remote, "PostgreSQLConnection")
  )
  "CREATE TABLE IF NOT EXISTS track_available
  (
    id INTEGER PRIMARY KEY AUTOINCREMENT, scheme_id INTEGER,
    classification_id INTEGER, species_id INTEGER, duration INTEGER,
    speed INTEGER, n INTEGER, FOREIGN KEY(scheme_id) REFERENCES scheme(id),
    FOREIGN KEY(classification_id) REFERENCES species(id),
    FOREIGN KEY(species_id) REFERENCES species(id)
  )" |>
      dbExecute(conn = local)
  "SELECT id, common_name FROM species" |>
    dbGetQuery(conn = local) -> species
  "WITH cte AS (
  SELECT scheme_id FROM track_available GROUP BY scheme_id
)
SELECT id, scheme
FROM scheme AS s
LEFT JOIN cte AS c ON s.id = c.scheme_id
WHERE c.scheme_id IS NULL" |>
    dbGetQuery(conn = local) -> scheme
  cli_progress_bar(name = "overview available tracks", total = nrow(scheme))
  while (nrow(scheme) > 0) {
    "WITH cte_detail AS (
  SELECT
    t.classification_id, o.species_id,
    FLOOR(
      2 * DATE_PART('minutes', t.timestamp_end - t.timestamp_start) +
      DATE_PART('seconds', t.timestamp_end - t.timestamp_start) / 30
    ) AS duration,
    FLOOR(t.airspeed / 5) AS speed
  FROM %1$s.track AS t
  LEFT JOIN %1$s.observation AS o ON t.id = o.track_id
),
cte_sum AS (
  SELECT classification_id, species_id, duration, speed, COUNT(*) AS n
  FROM cte_detail
  GROUP BY classification_id, species_id, duration, speed
)
SELECT l.classification, s.common_name, c.duration, c.speed, c.n
FROM cte_sum AS c
LEFT JOIN %1$s.classification AS l ON c.classification_id = l.id
LEFT JOIN config.species AS s ON c.species_id = s.id" |>
  sprintf(head(scheme$scheme, 1)) |>
      dbGetQuery(conn = remote) -> tracks
    tracks |>
      distinct(common_name = .data$classification) |>
      anti_join(species, by = "common_name") |>
      mutate(id = NA_integer_) |>
      dbAppendTable(conn = local, name = "species")
    tracks |>
      distinct(.data$common_name) |>
      filter(!is.na(.data$common_name)) |>
      anti_join(species, by = "common_name") |>
      mutate(id = NA_integer_) |>
      dbAppendTable(conn = local, name = "species")
    "SELECT id, common_name FROM species" |>
      dbGetQuery(conn = local) -> species
    tracks |>
      left_join(
        species |>
          select(species_id = "id", "common_name"),
        by = "common_name"
      ) |>
      left_join(
        species |>
          select(classification_id = "id", "classification" = "common_name"),
        by = c("classification")
      ) |>
      transmute(
        scheme_id = head(scheme$id, 1), .data$classification_id,
        .data$species_id, .data$duration, .data$speed, .data$n
      ) |>
      dbAppendTable(conn = local, name = "track_available")
    scheme <- tail(scheme, -1)
    cli_progress_update()
  }
  cli_progress_done()
}
