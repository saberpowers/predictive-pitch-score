#' Compute pitch value
#' 
#' Using pitch outcome model predictions, compute run value (expected linear weight).
#' 
#' @param pitch_pred dataframe of predictions from a pitch outcome model
#' @param count_value dataframe of count_value from \code{\link{compute_count_value}}
#' 
#' @export
#' 
compute_pitch_value <- function(pitch_pred, count_value) {

  # Treat an HBP as equal to a walk in run value
  count_value_hbp <- max(count_value$count_value)

  pitch_value <- pitch_pred |>
    dplyr::mutate(
      balls_plus_one = pre_balls + 1,
      strikes_plus_one = pre_strikes + 1,
      strikes_after_foul = ifelse(pre_strikes == 2, 2, pre_strikes + 1)
    ) |>
    # Attach count linear weight resulting from a ball
    dplyr::left_join(count_value,
      by = c("pre_balls" = "balls", "pre_strikes" = "strikes")
    ) |>
    # Attach count linear weight resulting from a ball
    dplyr::left_join(count_value,
      by = c("balls_plus_one" = "balls", "pre_strikes" = "strikes"),
      suffix = c("", "_ball")
    ) |>
    # Attach count linear weight resulting from a strike
    dplyr::left_join(count_value,
      by = c("pre_balls" = "balls", "strikes_plus_one" = "strikes"),
      suffix = c("", "_strike")
    ) |>
    # Attach count linear weight resulting from a strike
    dplyr::left_join(count_value,
      by = c("pre_balls" = "balls", "strikes_after_foul" = "strikes"),
      suffix = c("", "_foul")
    ) |>
    # Compute pitch value
    dplyr::transmute(
      pitch_value_swing = (1 - prob_contact) * count_value_strike +
        prob_contact * ((1 - prob_fair) * count_value_foul + prob_fair * pred_hit) -
        count_value,
      pitch_value_take = prob_hbp * count_value_hbp +
        (1 - prob_hbp) * (prob_strike * count_value_strike + (1 - prob_strike) * count_value_ball) -
        count_value,
      pitch_value = prob_swing * pitch_value_swing + (1 - prob_swing) * pitch_value_take
    )

  return(pitch_value)
}