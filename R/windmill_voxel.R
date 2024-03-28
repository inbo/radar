#' Get the number of tracks per voxels around the windmills
#' @inheritParams add_vleemo_observed_track
#' @inheritParams equal_time_track
#' @inheritParams voxel_around_centre
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom tidyr complete unnest
windmill_voxel <- function(
  local, max_distance = 200, voxel_size = rep(max_distance / 5, 3),
  voxel_box = matrix(c(-10, -10, 0, 10, 10, 20), nrow = 3), rate = 2
) {
  assert_that(inherits(local, "SQLiteConnection"))
  dbGetQuery(local, "SELECT id AS windmill, x, y FROM windmill") |>
    mutate(
      voxel = map2(
        .data$x, .data$y,
        ~voxel_around_centre(
          local = local, centre = data.frame(x = .x, y = .y),
          max_distance = max_distance, voxel_size = voxel_size,
          voxel_box = voxel_box, rate = rate
        )
      )
    ) |>
    select("windmill", "voxel") |>
    mutate(windmill = factor(.data$windmill)) |>
    unnest("voxel") |>
    complete(.data$windmill, .data$x, .data$y, .data$z, fill = list(n = 0))
}
