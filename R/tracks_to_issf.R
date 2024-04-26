#' Convert tracks to integrated step selection function data
#' Augments the tracks with a number of random generated step from the
#' null distribution of the `step_2d` and `delta_yaw`.
#' @param tracks a `data.frame` with a least the columns `equal_time_id`,
#' `step_2d` and `delta_yaw`.
#' @param n_sim Number of random alternatives
#' @export
#' @importFrom assertthat assert_that has_name is.count
#' @importFrom dplyr bind_rows filter inner_join mutate n select
#' @importFrom purrr map map2 map_dbl map_dfc
#' @importFrom Rfast rvonmises
#' @importFrom rlang .data
#' @importFrom stats rgamma
#' @importFrom stringr str_detect str_remove
#' @importFrom tibble rownames_to_column rowid_to_column
#' @importFrom tidyr pivot_longer
track_to_issf <- function(tracks, n_sim = 100) {
  assert_that(
    inherits(tracks, "data.frame"), has_name(tracks, "equal_time_id"),
    has_name(tracks, "step_2d"), has_name(tracks, "delta_yaw"), is.count(n_sim)
  )
  stopifnot(requireNamespace("INLA"))
  tracks |>
    mutate(equal_time_id = as.factor(.data$equal_time_id)) |>
    rowid_to_column() -> tracks
  null_step_2d <- INLA::inla(
    step_2d ~ f(equal_time_id, model = "iid"), family = "gamma",
    data = tracks, control.compute = list(config = TRUE)
  )
  null_delta_yaw <- INLA::inla(
    delta_yaw ~ 1, family = "circularnormal",
    data = tracks, control.compute = list(config = TRUE)
  )
  post_samp_step_2d <- INLA::inla.posterior.sample(n_sim, null_step_2d)
  post_samp_delta_yaw <- INLA::inla.posterior.sample(n_sim, null_delta_yaw)
  hyper_post_samp_step_2d <- INLA::inla.hyperpar.sample(n_sim, null_step_2d)
  hyper_post_samp_delta_yaw <- INLA::inla.hyperpar.sample(n_sim, null_delta_yaw)
  map(post_samp_step_2d, "latent") |>
    map2(sprintf("sim_%04i", seq_len(n_sim)), `colnames<-`) |>
    map_dfc(as.data.frame) |>
    rownames_to_column(var = "rowid") |>
    filter(str_detect(.data$rowid, "Predictor")) |>
    mutate(
      rowid = str_remove(.data$rowid, "Predictor:") |>
        as.integer()
    ) |>
    pivot_longer(-"rowid", names_to = "sim", values_to = "mu_step_2d") |>
    mutate(
      mu_step_2d = exp(.data$mu_step_2d),
      sim = str_remove(.data$sim, "sim_") |>
        as.integer(),
      shape_step_2d = hyper_post_samp_step_2d[
        .data$sim, "Precision parameter for the Gamma observations"
      ]
    ) |>
    transmute(
      .data$rowid, .data$sim,
      step_2d = rgamma(
        n(), shape = .data$shape_step_2d,
        scale = .data$mu_step_2d / .data$shape_step_2d
      )
    ) |>
    inner_join(
      map(post_samp_delta_yaw, "latent") |>
        map2(sprintf("sim_%04i", seq_len(n_sim)), `colnames<-`) |>
        map_dfc(as.data.frame) |>
        rownames_to_column(var = "rowid") |>
        filter(str_detect(.data$rowid, "Predictor")) |>
        mutate(
          rowid = str_remove(.data$rowid, "Predictor:") |>
            as.integer()
        ) |>
        pivot_longer(
          -"rowid", names_to = "sim", values_to = "mu_delta_yaw"
        ) |>
        mutate(
          mu_delta_yaw = 2 * atan(.data$mu_delta_yaw),
          sim = str_remove(.data$sim, "sim_") |>
            as.integer()
        ) |>
        group_by(.data$sim) |>
        transmute(
          .data$rowid, .data$sim,
          delta_yaw = map_dbl(
            .data$mu_delta_yaw, rvonmises, n = 1,
            k = exp(hyper_post_samp_delta_yaw[
              .data$sim[1],
              "Precision parameter for the Circular Normal observations"
            ])
          )
        ),
      by = c("rowid", "sim")
    ) |>
    inner_join(
      tracks |>
        mutate(observed = 0) |>
        select(-"step_2d", -"delta_yaw"),
      by = "rowid"
    ) |>
    bind_rows(
      tracks |>
        mutate(observed = 1, sim = 0)
    ) |>
    mutate(rowid = factor(.data$rowid), sim = factor(.data$sim))
}
