#' Compute count linear weight
#' 
#' Compute the average change in base-out run expectancy for all plate appearances that run
#' through each count.
#' 
#' @param pitch dataframe of pitch data from \code{\link{extract_season}}
#' @param event dataframe of event data from \code{\link{extract_season}}
#' @param base_out_run_exp dataframe of base-out run expectancy from
#'   \code{\link{compute_base_out_run_exp}}
#' 
#' @return a dataframe of `linear_weight` indexed by `balls` and `strikes`
#' 
compute_count_linear_weight <- function(pitch, event, base_out_run_exp) {

  data <- pitch |>
    dplyr::filter(pre_balls < 4, pre_strikes < 3) |>    # this can happen in the data
    dplyr::left_join(event, by = c("game_id", "event_index")) |>
    dplyr::select(game_id, event_index, pre_balls, pre_strikes, post_balls, post_strikes,
      dplyr::starts_with("pre_event_runner_"), pre_event_outs,
      dplyr::starts_with("post_runner_"), post_outs, runs_on_event
    ) |>
    # Join in pre-event run expectancy
    dplyr::mutate(
      runner_1b = !is.na(pre_event_runner_1b_id),
      runner_2b = !is.na(pre_event_runner_2b_id),
      runner_3b = !is.na(pre_event_runner_3b_id),
      outs = pre_event_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(pre_event_exp_runs = exp_runs) |>
    # Join in post-event run expectancy
    dplyr::mutate(
      runner_1b = !is.na(post_runner_1b_id),
      runner_2b = !is.na(post_runner_2b_id),
      runner_3b = !is.na(post_runner_3b_id),
      outs = post_outs
    ) |>
    dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
    dplyr::rename(post_exp_runs = exp_runs)

  # Compute average change in run expectancy by count
  nonterminal_linear_weight <- data |>
    dplyr::group_by(balls = pre_balls, strikes = pre_strikes) |>
    dplyr::summarize(
      linear_weight = mean(runs_on_event + post_exp_runs - pre_event_exp_runs),
      .groups = "drop"
    )

  walk_linear_weight <- data |>
    dplyr::filter(post_balls == 4) |>
    dplyr::group_by(balls = post_balls, strikes = post_strikes) |>
    dplyr::summarize(
      n = dplyr::n(),
      linear_weight = mean(runs_on_event + post_exp_runs - pre_event_exp_runs),
      .groups = "drop"
    ) |>
    dplyr::mutate(linear_weight = weighted.mean(linear_weight, w = n)) |>
    dplyr::select(balls, strikes, linear_weight)

  strikeout_linear_weight <- data |>
    dplyr::filter(post_strikes == 3) |>
    dplyr::group_by(balls = post_balls, strikes = post_strikes) |>
    dplyr::summarize(
      n = dplyr::n(),
      linear_weight = mean(runs_on_event + post_exp_runs - pre_event_exp_runs),
      .groups = "drop"
    ) |>
    dplyr::mutate(linear_weight = weighted.mean(linear_weight, w = n)) |>
    dplyr::select(balls, strikes, linear_weight)

  count_linear_weight <- dplyr::bind_rows(
    nonterminal_linear_weight,
    walk_linear_weight,
    strikeout_linear_weight
  )
  
  return(count_linear_weight)
}
