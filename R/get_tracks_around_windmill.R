#' extracts tracks nearest to windmills
#' @inheritParams add_vleemo_observed_track
#' @inheritParams equal_time_track
#' @inheritParams voxel_around_centre
#' @inheritParams get_tracks_around_centre
#' @export
#' @importFrom assertthat assert_that has_name is.count noNA
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom dplyr anti_join bind_rows distinct group_by mutate select
#' semi_join slice_min
#' @importFrom RSQLite dbGetQuery
get_tracks_around_windmill <- function(
  local, max_distance = 200, max_height = 200, max_tracks = 100,
  relevant = 1, rate = 2, operator
) {
  assert_that(inherits(local, "SQLiteConnection"))
  if (missing(operator)) {
    "SELECT id AS windmill_id, x, y FROM windmill" |>
      dbGetQuery(conn = local) -> centres
  } else {
    assert_that(is.string(operator), noNA(operator))
    "SELECT id AS windmill_id, x, y FROM windmill WHERE operator == \"%s\"" |>
      sprintf(operator) |>
      dbGetQuery(conn = local) -> centres
  }
  data.frame(equal_time_id = integer(0)) -> tracks
  cli_progress_bar("read tracks per windmill", total = nrow(centres))
  for (i in seq_len(nrow(centres))) {
    get_tracks_around_centre(
      local = local, centre = centres[i, ], max_distance = max_distance,
      max_height = max_height, max_tracks = max_tracks, relevant = relevant,
      rate = rate
    ) |>
      anti_join(tracks, by = "equal_time_id") |>
      bind_rows(tracks) -> tracks
    cli_progress_update()
  }
  cli_progress_done()
  tracks |>
    group_by(.data$equal_time_id) |>
    summarise(
      distance = min(sqrt(
        (.data$x - .data$windmill_x) ^ 2 + (.data$y - .data$windmill_y) ^ 2
      ))
    ) |>
    slice_min(.data$distance, n = max_tracks) |>
    semi_join(x = tracks, by = "equal_time_id")
}
