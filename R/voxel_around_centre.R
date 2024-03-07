#' Calculate the number of tracks through a voxel
#'
#' A voxel is the 3D equivalent of a pixel.
#' Counts the number of unique tracks going through a voxel.
#' The function rotates the tracks according to the average wind direction of
#' the track.
#' The x direction is parallel to the wind direction.
#' @inheritParams add_vleemo_observed_track
#' @param centre a data frame with a single row and x and y coordinates of the
#' centre.
#' Coordinates in CRS 31370
#' @param max_distance Use all equal time tracks where the minimum distance in
#' meters between the track and the centre is smaller than `max_distance`.
#' @param voxel_size Size in meters of the voxel.
#' @param voxel_box Defines the area around the centre for to set of voxels to
#' use.
#' Defined using the index number of the voxels.
#' Defaults to 10 voxels on both side of the centre in x and y and 20 voxels
#' above 0 in the z direction.
#' @importFrom assertthat assert_that has_name is.number noNA
#' @importFrom dplyr count distinct filter mutate select transmute
#' @importFrom purrr pmap
#' @importFrom RSQLite dbGetQuery
#' @importFrom tidyr unnest
#' @export
voxel_around_centre <- function(
  local, centre, max_distance = 200, voxel_size = rep(max_distance / 5, 3),
  voxel_box = matrix(c(-10, -10, 0, 10, 10, 20), nrow = 3)
) {
  assert_that(
    inherits(local, "SQLiteConnection"), inherits(centre, "data.frame"),
    has_name(centre, "x"), has_name(centre, "y"), nrow(centre) == 1,
    is.number(centre$x), is.number(centre$y), noNA(centre$x), noNA(centre$y),
    is.number(max_distance), noNA(max_distance), max_distance > 0,
    is.numeric(voxel_size), length(voxel_size) == 3, noNA(voxel_size),
    all(voxel_size > 0), is.matrix(voxel_box), is.numeric(voxel_box[1, 1]),
    identical(dim(voxel_box), c(3L, 2L)), noNA(voxel_box)
  )
  "WITH cte AS (
  SELECT DISTINCT equal_time_id
  FROM track_equal_time_point
  WHERE SQRT(POWER(x - %1$f, 2) + POWER(y - %2$f, 2)) <= %3$f
),
cte_centre AS (
  SELECT
    tp.equal_time_id, tp.t, tp.x - %1$f AS x0, tp.y - %2$f AS y0, tp.z
  FROM track_equal_time_point AS tp
  INNER JOIN cte AS c ON tp.equal_time_id = c.equal_time_id
),
cte_direction AS (
  SELECT
    c.equal_time_id, c.t, c.z,
    ATAN2(c.y0, c.x0) - radians(90 - t.wind_direction) AS direction,
    SQRT(POWER(c.x0, 2) + POWER(c.y0, 2)) AS distance
  FROM cte_centre AS c
  INNER JOIN track_equal_time AS te ON c.equal_time_id = te.id
  INNER JOIN track_time AS t ON te.track_id = t.id
),
cte_voxel AS (
  SELECT
    equal_time_id, t, distance * COS(direction) / %4$f AS xv,
    distance * SIN(direction) / %5$f AS yv, z / %6$f AS zv
  FROM cte_direction
)
SELECT
  v0.equal_time_id, v0.xv AS x0, v1.xv AS x1, v0.yv AS y0, v1.yv AS y1,
  v0.zv AS z0, v1.zv AS z1
FROM cte_voxel AS v0
INNER JOIN cte_voxel AS v1 ON
  v0.t = v1.t - 2 AND v0.equal_time_id = v1.equal_time_id
WHERE
  ((%7$f <= v0.xv AND v0.xv <= %8$s) OR (%7$f <= v1.xv AND v1.xv <= %8$s)) AND
  ((%9$f <= v0.yv AND v0.yv <= %10$s) OR (%9$f <= v1.yv AND v1.yv <= %10$s)) AND
  ((%11$f <= v0.zv AND v0.zv <= %12$s) OR (%11$f <= v1.zv AND v1.zv <= %12$s))
" |>
    sprintf(
      centre$x, centre$y, max_distance, voxel_size[1], voxel_size[2],
      voxel_size[3], voxel_box[1, 1], voxel_box[1, 2], voxel_box[2, 1],
      voxel_box[2, 2], voxel_box[3, 1], voxel_box[3, 2]
    ) |>
    dbGetQuery(conn = local) |>
    mutate(
      dx = .data$x1 - .data$x0, dy = .data$y1 - .data$y0,
      dz = .data$z1 - .data$z0,
      x = pmap(
        list(
          x0 = .data$x0, x1 = .data$x1,
          l = ceiling(1 + sqrt(.data$dx ^ 2 + .data$dy ^ 2 + .data$dz ^ 2))
        ),
        function(x0, x1, l) {
          seq(x0, x1, length = l)
        }
      )
    ) |>
    unnest("x") |>
    transmute(
      .data$equal_time_id, .data$x,
      y = round(.data$y0 + (.data$x - .data$x0) * .data$dy / .data$dx),
      z = round(.data$z0 + (.data$x - .data$x0) * .data$dz / .data$dx),
      x = round(.data$x)
    ) |>
    distinct(.data$equal_time_id, .data$x, .data$y, .data$z) |>
    count(.data$x, .data$y, .data$z) |>
    filter(
      voxel_box[1, 1] <= .data$x, .data$x <= voxel_box[1, 2],
      voxel_box[2, 1] <= .data$y, .data$y <= voxel_box[2, 2],
      voxel_box[3, 1] <= .data$z, .data$z <= voxel_box[3, 2]
    )
}
