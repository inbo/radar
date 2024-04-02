#' Extract tracks nearest to a given position
#' @inheritParams add_vleemo_observed_track
#' @inheritParams equal_time_track
#' @inheritParams voxel_around_centre
#' @param max_height Only use track points with an altitude lower than
#' `max_height`.
#' Defaults to 200 meter.
#' @param max_tracks Return only the nearest `max_tracks` tracks.
#' @param relevant The value of the `relevant` variable of the `species` table
#' in the database.
#' Select only tracks from species with a value equal or larger than this
#' number.
#' @param rate The time difference between points to calculate the step length
#' and turning angles.
#' @export
#' @importFrom assertthat assert_that has_name is.count noNA
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom RSQLite dbGetQuery
get_tracks_around_centre <- function(
  local, centre, max_distance = 200, max_height = 200, max_tracks = 100,
  relevant = 1, rate = 2
) {
  assert_that(
    inherits(local, "SQLiteConnection"), inherits(centre, "data.frame"),
    has_name(centre, "x"), has_name(centre, "y"), is.number(max_distance),
    noNA(max_distance), max_distance > 0, is.number(max_height),
    noNA(max_height), max_height > 0, is.count(max_tracks), is.count(relevant),
    is.count(rate)
  )
  "WITH cte_nearest AS (
  SELECT p.equal_time_id
  FROM track_equal_time_point AS p
  INNER JOIN track_equal_time AS te ON te.id = p.equal_time_id
  INNER JOIN track_time AS tt ON tt.id = te.track_id
  INNER JOIN species AS s ON tt.species_id = s.id
  WHERE s.relevant >= %i AND p.z <= %i  AND
      SQRT(POWER(p.x - %f, 2) + POWER(p.y - %f, 2)) <= %f
  GROUP BY p.equal_time_id
  LIMIT %i
),
cte_windmill_all AS (
  SELECT p.id AS point_id, w.id AS windmill_id
  FROM cte_nearest AS c
  INNER JOIN track_equal_time_point AS p ON c.equal_time_id = p.equal_time_id
  CROSS JOIN windmill AS w
  GROUP BY point_id
  HAVING MIN(SQRT(POWER(p.x - w.x, 2) + POWER(p.y - w.y, 2)))
)
SELECT
  p1.equal_time_id, p1.t, p1.x, p1.y, p1.z,
  w.x AS windmill_x, w.y AS windmill_y,
  SQRT(POWER(p2.x - p1.x, 2) + POWER(p2.y - p1.y, 2)) AS step_2d,
  ATAN2(p1.y - p0.y, p1.x - p0.x) AS yaw,
  ATAN2(p2.y - p1.y, p2.x - p1.x) - ATAN2(p1.y - p0.y, p1.x - p0.x) AS delta_yaw
FROM cte_nearest AS c
INNER JOIN track_equal_time_point AS p0 ON c.equal_time_id = p0.equal_time_id
INNER JOIN track_equal_time_point AS p1
  ON p0.equal_time_id = p1.equal_time_id AND p0.t + %i = p1.t
INNER JOIN track_equal_time_point AS p2
  ON p1.equal_time_id = p2.equal_time_id AND p1.t + %i = p2.t
INNER JOIN cte_windmill_all AS cw ON p1.id = cw.point_id
INNER JOIN windmill AS w ON cw.windmill_id = w.id" |>
    sprintf(
      relevant, max_height, centre$x, centre$y, max_distance, max_tracks, rate,
      rate
    ) |>
    dbGetQuery(conn = local) |>
    mutate(
      delta_yaw = .data$delta_yaw + pi * ifelse(
        .data$delta_yaw > pi, -2, ifelse(.data$delta_yaw < -pi, 2, 0)
      )
    )
}
