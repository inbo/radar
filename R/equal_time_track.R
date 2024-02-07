#' Convert a irregular track into a track with equal time intervals
#'
#' The resulting track has
#'
#'  - the part of the track (`part`).
#'    We split the part at the points where the ground speed is above 30 meter
#'    per second.
#'  - the space-time coordinates (`t`, `x`, `y` and `z`)
#'  - the step length in 2D and 3D (`step_2d` and `step_3d`)
#'  - the heading in the x-y plane (`yaw`)
#'  - the heading in the plane between the z-axis and the direction in
#'    the x-y plane (`pitch`)
#'  - the change in yaw and pitch between two consecutive points (`delta_yaw`
#'    and `delta_pitch`)
#' @param raw_track A single track as a data.frame with projected space-time
#' coordinates.
#' Coordinates should be in meters, the time in seconds.
#' @param rate the time between two points in the output track.
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom dplyr arrange bind_rows filter group_by lag lead mutate
#' inner_join rename select slice_max slice_min summarise transmute ungroup
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
equal_time_track <- function(raw_track, rate = 2) {
  min_delta_t <- 0.9
  max_speed <- 30
  min_duration <- 60
  assert_that(
    inherits(raw_track, "data.frame"), has_name(raw_track, "x"),
    has_name(raw_track, "y"), has_name(raw_track, "z"),
    has_name(raw_track, "t"), is.number(rate), rate > min_delta_t
  )
  raw_track |>
    filter(
      pmin(.data$t - lag(.data$t), lead(.data$t) - .data$t, na.rm = TRUE) >
        min_delta_t
    ) -> raw_track
  groundspeed <- sqrt(diff(raw_track$x) ^ 2 + diff(raw_track$y) ^ 2) /
    diff(raw_track$t)
  raw_track |>
    mutate(
      part = c(NA, groundspeed) |>
        pmax(c(groundspeed, NA), na.rm = TRUE),
      part = cumsum(.data$part > max_speed)
    ) -> raw_track
  raw_track |>
    group_by(.data$part) |>
    summarise(start = min(.data$t), end = max(.data$t)) |>
    mutate(
      start = ceiling(.data$start / rate) * rate,
      end = floor(.data$end / rate) * rate
    ) |>
    filter(.data$end - .data$start >= min_duration) -> parts
  raw_track |>
    mutate(tr = ceiling(.data$t / rate) * rate) |>
    group_by(.data$part, .data$tr) |>
    slice_max(.data$t, n = 1) |>
    ungroup() |>
    rename(t0 = "t", x0 = "x", y0 = "y", z0 = "z") |>
    inner_join(
      x = parts |>
        transmute(
          .data$part, tr = map2(.data$start, .data$end, ~seq(.x, .y, by = rate))
        ) |>
        unnest("tr"),
      by = c("part", "tr")
    ) |>
    inner_join(
      raw_track |>
        mutate(tr = floor(.data$t / rate) * rate) |>
        group_by(.data$part, .data$tr) |>
        slice_min(.data$t, n = 1) |>
        ungroup() |>
        rename(t1 = "t", x1 = "x", y1 = "y", z1 = "z"),
      by = c("part", "tr")
    ) -> spaced
  spaced |>
    filter(.data$t0 < .data$t1) |>
    transmute(
      .data$part, t = .data$tr,
      x = .data$x0 + (.data$x1 - .data$x0) * (.data$tr - .data$t0) /
        (.data$t1 - .data$t0),
      y = .data$y0 + (.data$y1 - .data$y0) * (.data$tr - .data$t0) /
        (.data$t1 - .data$t0),
      z = .data$z0 + (.data$z1 - .data$z0) * (.data$tr - .data$t0) /
        (.data$t1 - .data$t0)
    ) |>
    bind_rows(
      spaced |>
        filter(.data$t0 == .data$t1) |>
        select("part", t = "tr", x = "x0", y = "y0", z = "z0")
    ) |>
    arrange(.data$t) |>
    group_by(.data$part) |>
    mutate(
      dx = .data$x - lead(.data$x), dy = .data$y - lead(.data$y),
      dz = .data$z - lead(.data$z),
      step_2d = sqrt(.data$dx ^ 2 + .data$dy ^ 2),
      step_3d = sqrt(.data$dx ^ 2 + .data$dy ^ 2 + .data$dz ^ 2),
      yaw = atan2(.data$dy, .data$dx),
      pitch = atan2(.data$dz, .data$step_2d),
      delta_yaw = (.data$yaw - lag(.data$yaw) + pi) %% (2 * pi) - pi,
      delta_pitch = .data$pitch - lag(.data$pitch)
    ) |>
    filter(!is.na(.data$delta_yaw)) |>
    ungroup() |>
    select(
      "part", "t", "x", "y", "z", "step_2d", "step_3d", "yaw", "pitch",
      "delta_yaw", "delta_pitch"
    )
}
