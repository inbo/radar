#' Convert a irregular track into a track with equal time intervals
#'
#' The resulting track has
#'
#'  - the part of the track (`part`).
#'    We split the part at the points where the ground speed is above 30 meter
#'    per second.
#'  - the space-time coordinates (`t`, `x`, `y` and `z`)
#' @param raw_track A single track as a data.frame with projected space-time
#' coordinates.
#' Coordinates should be in meters, the time in seconds.
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom dplyr across arrange bind_rows filter group_by inner_join lag
#' lead mutate rename select slice_max slice_min summarise transmute ungroup
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export
equal_time_track <- function(raw_track) {
  min_delta_t <- 0.9
  max_speed <- 30
  min_duration <- 60
  max_gap <- 5
  assert_that(
    inherits(raw_track, "data.frame"), has_name(raw_track, "x"),
    has_name(raw_track, "y"), has_name(raw_track, "z"), has_name(raw_track, "t")
  )
  raw_track |>
    filter(
      pmin(.data$t - lag(.data$t), lead(.data$t) - .data$t, na.rm = TRUE) >
        min_delta_t
    ) |>
    mutate(
      part = cumsum(
        .data$t - lag(.data$t, 1, default = .data$t[1]) > max_gap
      )
    ) -> raw_track
  raw_track |>
    group_by(.data$part) |>
    mutate(
      groundspeed = c(
        0, sqrt(diff(.data$x) ^ 2 + diff(.data$y) ^ 2) / diff(.data$t)
      ),
      yaw = atan2(lag(.data$y, 1) - .data$y, lag(.data$x, 1) - .data$x),
      delta_yaw = lead(.data$yaw, 1) - .data$yaw,
      delta_yaw = .data$delta_yaw + pi *
        ifelse(.data$delta_yaw > pi, -2, ifelse(.data$delta_yaw < -pi, 2, 0))
    ) |>
    ungroup() |>
    mutate(
      part = cumsum(
        c(0, diff(.data$part)) + (.data$groundspeed > max_speed) +
          !(is.na(.data$delta_yaw) | abs(.data$delta_yaw) <= pi * 7 / 8)
      )
    ) -> raw_track
  raw_track |>
    group_by(.data$part) |>
    summarise(start = min(.data$t), end = max(.data$t), .groups = "drop") |>
    filter(.data$end - .data$start >= min_duration) -> parts
  raw_track |>
    mutate(tr = ceiling(.data$t)) |>
    group_by(.data$part, .data$tr) |>
    slice_max(.data$t, n = 1) |>
    ungroup() |>
    select("part", "tr", t0 = "t", x0 = "x", y0 = "y", z0 = "z") |>
    inner_join(
      x = parts |>
        transmute(
          .data$part,
          tr = map2(ceiling(.data$start), floor(.data$end), ~seq(.x, .y))
        ) |>
        unnest("tr"),
      by = c("part", "tr")
    ) |>
    inner_join(
      raw_track |>
        mutate(tr = floor(.data$t)) |>
        group_by(.data$part, .data$tr) |>
        slice_min(.data$t, n = 1) |>
        ungroup() |>
        select("part", "tr", t1 = "t", x1 = "x", y1 = "y", z1 = "z"),
      by = c("part", "tr")
    ) |>
    mutate(across(c("part", "tr"), as.integer)) -> spaced
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
    arrange(.data$t)
}
