#' Simulate pitch score
#' 
#' Draw samples from the estimated pitch distribution for a specified pitcher, and run them through
#' the pitch outcome model to report the average predicted pitch value.
#' 
#' @param pitcher_id integer, ID of pitcher to simulate (must be length-1)
#' @param n integer, number of pitches to simulate
#' @param context a dataframe with the following columns:
#'   `bat_side`, `pre_balls`, `pre_strikes`, `strike_zone_top`, `strike_zone_bottom`
#' @param pitch_distrib_model a fitted "pitch_distrib_model" object
#' @param pitch_outcome_model a fitted "pitch_outcome_model" object
#' 
#' @return a scalar pitch score
#' 
#' @export
#' 
simulate_pitch_score <- function(pitcher_id, n, context, pitch_distrib_model, pitch_outcome_model) {

  simmed_pitch <- simulate_pitches(
    model = pitch_distrib_model,
    pitcher_id = pitcher_id,
    n = n,
    context = context
  ) |>
    get_trackman_metrics()

  pred <- predict(
    object = pitch_outcome_model,
    newpitch = simmed_pitch
  )

  pred_pitch_score <- mean(pred$pitch_value)

  return(pred_pitch_score)
}
