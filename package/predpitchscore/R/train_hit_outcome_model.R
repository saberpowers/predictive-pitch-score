#' Train hit outcome model
#' 
#' Fit an XGBoost model to predict change in base-out run expectancy based on
#' launch speed (exit velo), launch angle and hit bearing.
#' 
#' @param pitch dataframe of pitch data from \code{\link{extract_season}}
#' @param event dataframe of event data from \code{\link{extract_season}}
#' @param base_out_run_exp dataframe of base-out run expectancy from
#'   \code{\link{compute_base_out_run_exp}}
#' 
#' @return a fitted xgb.Booster object
#' 
#' @export
#' 
train_hit_outcome_model <- function(pitch, event, base_out_run_exp) {

  data <- pitch |>
    # We only care about pitches for which we observe launch speed
    dplyr::filter(!is.na(launch_speed)) |>
    dplyr::left_join(event, by = c("game_id", "event_index")) |>
    # Join in pre-play expected runs (based on base-out state prior to last pitch of PA)
    dplyr::mutate(
      runner_1b = !is.na(pre_play_runner_1b_id),
      runner_2b = !is.na(pre_play_runner_2b_id),
      runner_3b = !is.na(pre_play_runner_3b_id),
      outs = pre_play_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(exp_runs_pre = exp_runs) |>
    # Join in post-event expected runs
    dplyr::mutate(
      runner_1b = !is.na(post_runner_1b_id),
      runner_2b = !is.na(post_runner_2b_id),
      runner_3b = !is.na(post_runner_3b_id),
      outs = post_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(exp_runs_post = exp_runs) |>
    dplyr::transmute(
      launch_speed,
      launch_angle,
      # The GUMBO documentation specifies that home plate is at x = 125 but does not specify the
      # y-coordinate of home plate (apparently it varies by stadium). Based on the largest
      # y-coordinates for ground balls, home plate seems to be somewhere around y > 200 (as y
      # increases, we move in the direction from center field toward home plate). Within the region
      # y > 200, by far the greatest concentration of ground ball coordinates is at y = 204.5
      # (extremely few y > 204.5), so we'll treat y = 205 as home plate.
      hit_bearing = (atan((hit_coord_x - 125) / (205 - hit_coord_y))) * 180 / pi, # radian to degree
      exp_runs_diff = runs_on_event + exp_runs_post - exp_runs_pre
    )
  
  covariate_matrix <- data |>
    dplyr::select(launch_speed, launch_angle, hit_bearing) |>
    as.matrix()

  model <- xgboost::xgboost(
    data = covariate_matrix,
    label = data$exp_runs_diff,
    nrounds = 10, # temporarily set to 10 for testing (150 will be better)
    params = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "reg:squarederror"),
    verbose = 0
  )

  model$pred <- predict(model, newdata = covariate_matrix)

  return(model)
}
