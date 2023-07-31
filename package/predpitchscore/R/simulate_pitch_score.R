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
  
  simmed_pitch$is_rhb <- as.numeric(simmed_pitch$bat_side=="R")

    pred <- predict(
    object = pitch_outcome_model,
    newpitch = simmed_pitch
  )
  actual_hbp=pred$prob_hbp*(1-pred$prob_swing)
  actual_strike=(1-pred$prob_swing-actual_hbp)*pred$prob_strike
  actual_contact=pred$prob_swing*pred$prob_contact
  actual_fair=pred$prob_fair*actual_contact
  actual_hit=pred$pred_hit*actual_contact
  mean_prob_swing=mean(pred$prob_swing)
  mean_prob_hbp=sum(actual_hbp)/sum(1-pred$prob_swing)
  mean_prob_strike=sum(actual_strike)/sum(1-pred$prob_swing-actual_hbp)
  mean_prob_contact=sum(actual_contact)/sum(pred$prob_swing)
  mean_prob_fair=sum(actual_fair)/sum(actual_contact)
  mean_pred_hit=sum(actual_hit)/sum(actual_fair)
  mean_pitch_value=mean(pred$pitch_value)
  mean_pred_value=mean(pred$pred_value)
  pred_model <- c(mean_prob_swing,mean_prob_hbp,mean_prob_strike,mean_prob_contact,
                  mean_prob_fair,mean_pred_hit,mean_pitch_value,mean_pred_value)
  
  return(pred_model)
}
