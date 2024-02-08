#' Append generic movement parameters
#' @inheritParams add_vleemo_observed_track
#' @importFrom assertthat assert_that
#' @importFrom RSQLite dbAppendTable dbClearResult dbGetQuery dbSendQuery
#' @export
generic_movement <- function(local) {
  stopifnot(requireNamespace("INLA", quietly = TRUE))
  assert_that(inherits(local, "SQLiteConnection"))
  "CREATE TABLE IF NOT EXISTS generic_movement
(
  id INTEGER PRIMARY KEY, step_2d_eta_mean NUMERIC, step_2d_eta_sd NUMERIC,
  step_2d_alpha_mean NUMERIC, step_2d_alpha_sd NUMERIC,
  step_3d_eta_mean NUMERIC, step_3d_eta_sd NUMERIC, step_3d_alpha_mean NUMERIC,
  step_3d_alpha_sd NUMERIC, yaw_eta_mean NUMERIC, yaw_eta_sd NUMERIC,
  yaw_alpha_mean NUMERIC, yaw_alpha_sd NUMERIC, pitch_eta_mean NUMERIC,
  pitch_eta_sd NUMERIC, pitch_alpha_mean NUMERIC, pitch_alpha_sd NUMERIC,
  FOREIGN KEY(id) REFERENCES track_equal_time(id)
)" |>
    dbSendQuery(conn = local) |>
    dbClearResult()
  while (TRUE) {
    "WITH cte AS (
  SELECT t.id
  FROM track_equal_time AS t
  LEFT JOIN generic_movement AS c ON t.id = c.id
  WHERE part >= 0 AND step_2d_eta_mean IS NULL AND yaw_eta_mean IS NULL
  LIMIT 1
)
SELECT t.id, p.step_2d, p.step_3d, p.delta_yaw, p.delta_pitch
FROM cte AS t
INNER JOIN track_equal_time_point AS p ON t.id = p.equal_time_id" |>
      dbGetQuery(conn = local) -> dataset
    if (nrow(dataset) == 0) {
      break
    }
    message(dataset$id[1], " step_2d", appendLF = FALSE)
    m_step_2d <- try(INLA::inla(
      step_2d ~ 1, family = "weibull", data = dataset, inla.mode = "classic",
      control.family = list(list(variant = 1))
    ))
    message(" yaw", appendLF = FALSE)
    m_yaw <- try(INLA::inla(
      delta_yaw ~ 1, family = "circularnormal", data = dataset,
      inla.mode = "classic"
    ))
    message(" step_3d", appendLF = FALSE)
    m_step_3d <- try(INLA::inla(
      step_3d ~ 1, family = "weibull", data = dataset, inla.mode = "classic",
      control.family = list(list(variant = 1))
    ))
    message(" pitch")
    m_pitch <- try(INLA::inla(
      delta_pitch ~ 1, family = "circularnormal", data = dataset,
      inla.mode = "classic"
    ))
    data.frame(
      id = dataset$id[1],
      step_2d_eta_mean = ifelse(
        inherits(m_step_2d, "try-error"), NA, m_step_2d$summary.fixed$mean
      ),
      step_2d_eta_sd = ifelse(
        inherits(m_step_2d, "try-error"), NA, m_step_2d$summary.fixed$sd
      ),
      step_2d_alpha_mean = ifelse(
        inherits(m_step_2d, "try-error"), NA, m_step_2d$summary.hyperpar$mean
      ),
      step_2d_alpha_sd = ifelse(
        inherits(m_step_2d, "try-error"), NA, m_step_2d$summary.hyperpar$sd
      ),
      step_3d_eta_mean = ifelse(
        inherits(m_step_3d, "try-error"), NA, m_step_3d$summary.fixed$mean
      ),
      step_3d_eta_sd = ifelse(
        inherits(m_step_3d, "try-error"), NA, m_step_3d$summary.fixed$sd
      ),
      step_3d_alpha_mean = ifelse(
        inherits(m_step_3d, "try-error"), NA, m_step_3d$summary.hyperpar$mean
      ),
      step_3d_alpha_sd = ifelse(
        inherits(m_step_3d, "try-error"), NA, m_step_3d$summary.hyperpar$sd
      ),
      yaw_eta_mean = ifelse(
        inherits(m_yaw, "try-error"), NA, m_yaw$summary.fixed$mean
      ),
      yaw_eta_sd = ifelse(
        inherits(m_yaw, "try-error"), NA, m_yaw$summary.fixed$sd
      ),
      yaw_alpha_mean = ifelse(
        inherits(m_yaw, "try-error"), NA, m_yaw$summary.hyperpar$mean
      ),
      yaw_alpha_sd = ifelse(
        inherits(m_yaw, "try-error"), NA, m_yaw$summary.hyperpar$sd
      ),
      pitch_eta_mean = ifelse(
        inherits(m_pitch, "try-error"), NA, m_pitch$summary.fixed$mean
      ),
      pitch_eta_sd = ifelse(
        inherits(m_pitch, "try-error"), NA, m_pitch$summary.fixed$sd
      ),
      pitch_alpha_mean = ifelse(
        inherits(m_pitch, "try-error"), NA, m_pitch$summary.hyperpar$mean
      ),
      pitch_alpha_sd = ifelse(
        inherits(m_pitch, "try-error"), NA, m_pitch$summary.hyperpar$sd
      )
    ) |>
      dbAppendTable(conn = local, name = "generic_movement")
  }
}
