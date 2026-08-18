#' Simulate pitches according to pitch distribution model
#' 
#' For a single pitcher, we draw pitch characteristic z-scores based on their estimated Rho matrix,
#' and then we re-center/re-scale those simulated z-scores based on the context (which is itself
#' drawn with replacement from the dataframe provided) and the pitcher's estimated parameters.
#' 
#' @param model a fitted "pitch_distrib_model" object
#' @param pitcher_id integer, ID of pitcher to simulate (must be length-1)
#' @param n integer, number of pitches to simulate
#' @param context a dataframe with the following columns:
#'   `bat_side`, `balls`, `strikes`, `strike_zone_top`, `strike_zone_bottom`
#' 
#' @return a dataframe of simulated pitch characteristics
#' 
#' @export
#' 
simulate_pitches <- function(model, pitcher_id, n, context) {

  # Unpack pitch distribution model ----

  league_params <- model$league_params
  pitch_char_vec <- model$pitch_char_vec

  pitcher_coef <- expand.grid(
    pitcher_id = model$pitcher_hand$pitcher_id,
    pitch_char = pitch_char_vec
  ) |>
    tibble::add_column(
      mu = c(model$map$mu),
      pi = c(model$map$pivar),
      nu = c(model$map$nu),
      xi = c(model$map$xi),
      sigma = c(model$map$sigma)
    )

  count_coef <- expand.grid(
    bsh_num = 1:24,
    pitch_char = pitch_char_vec
  ) |> tibble::add_column(
    lambda = c(model$map$lambda),
    zeta = c(model$map$zeta)
  )

  general_coef <- tibble::tibble(
    pitch_char = pitch_char_vec,
    theta = c(model$map$theta),
    kappa = c(model$map$kappa)
  )

  all_rho <- array(model$map$Rho,
    dim = c(nrow(model$pitcher_hand), length(pitch_char_vec), length(pitch_char_vec))
  )
  rho <- all_rho[which(model$pitcher_hand$pitcher_id == pitcher_id), , ]

  pitch_char_params <- league_params |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    # Filter down to mean and sd for pitch characteristics
    dplyr::filter(substring(name, 1, 2) %in% pitch_char_vec) |>
    tidyr::separate(name, into = c("pitch_char", "param")) |>
    tidyr::pivot_wider(names_from = param) |>
    # Rename these mean and SD columns to distinguish from player means and SDs
    dplyr::rename(league_mean = mean, league_sd = sd)


  # Simulate pitches ----

  simmed_context <- context |>
    # Make sure `context` has exactly the columns we need
    dplyr::select(bat_side, balls, strikes, strike_zone_top, strike_zone_bottom) |>
    # Sample n different contexts with replacement
    dplyr::slice(sample(1:dplyr::n(), size = n, replace = TRUE)) |>
    dplyr::mutate(sim_num = 1:n) |>
    # Replicate the table once for each pitch characteristic we're modelling
    dplyr::slice(rep(1:dplyr::n(), times = length(pitch_char_vec)))

  simmed_chol_scores <- matrix(
    data = rnorm(n * length(pitch_char_vec)),
    nrow = n,
    ncol = length(pitch_char_vec)
  )

  simmed_z_scores <- simmed_chol_scores %*% t(rho)
  
  simmed_pitch <- simmed_context |>
    dplyr::mutate(
      pitcher_id = pitcher_id,
      pitch_char = rep(pitch_char_vec, each = n),
      z_score = c(simmed_z_scores)
    ) |>
    dplyr::left_join(model$pitcher_hand, by = "pitcher_id") |>
    dplyr::mutate(
      same_hand = as.numeric(pitch_hand == bat_side),
      bsh_num = same_hand * 12 + balls * 3 + strikes + 1
    ) |>
    dplyr::left_join(pitcher_coef, by = c("pitcher_id", "pitch_char")) |>
    dplyr::left_join(count_coef, by = c("bsh_num", "pitch_char")) |>
    dplyr::left_join(general_coef, by = "pitch_char") |>
    dplyr::left_join(pitch_char_params, by = "pitch_char") |>
    dplyr::mutate(
      mean = mu +                                               # pitcher intercept
        pi * (same_hand - league_params$same_hand_mean) +       # pitcher slope for same_hand
        nu * (balls - league_params$balls_mean) +               # pitcher slope for balls
        xi * (strikes - league_params$strikes_mean) +           # pitcher slope for strikes
        lambda +                                                # count intercept
        theta * (strike_zone_top - league_params$strike_zone_top_mean) / league_params$strike_zone_top_sd +           # slope for strike_zone_top
        kappa * (strike_zone_bottom - league_params$strike_zone_bottom_mean) / league_params$strike_zone_bottom_sd,   # slope for strike_zone_bottom
      sd = sigma * zeta   # pitcher effect x count effect
    ) |>
    dplyr::mutate(
      # First apply the player-specific mean and SD to get a player-specific z-score
      player_z_score = mean + sd * z_score,
      # Then convert that player-specific z-score to an actual number using league mean and SD
      value = league_mean + league_sd * player_z_score,
      # Reverse x-coordinate flipping for LHP
      value = ifelse(pitch_hand == "L" & pitch_char %in% c("ax", "bx", "cx"), -1, 1) * value
    ) |>
    dplyr::select(sim_num, bat_side, balls, strikes, pitch_char, value, strike_zone_top, strike_zone_bottom) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = value)

  return(simmed_pitch)
}
