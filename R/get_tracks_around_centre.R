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
#' @export
#' @importFrom assertthat assert_that has_name is.count noNA
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
  WHERE
    s.relevant >= %i AND te.rate = %i AND p.z <= %i AND
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
  p.equal_time_id, p.t, p.x, p.y, p.z, p.step_2d, p.yaw, p.delta_yaw,
  w.x AS windmill_x, w.y AS windmill_y
FROM cte_windmill_all AS c
INNER JOIN track_equal_time_point AS p ON c.point_id = p.id
INNER JOIN windmill AS w ON c.windmill_id = w.id
" |>
  sprintf(
    relevant, rate, max_height, centre$x, centre$y, max_distance, max_tracks
  ) |>
  dbGetQuery(conn = local)
}
