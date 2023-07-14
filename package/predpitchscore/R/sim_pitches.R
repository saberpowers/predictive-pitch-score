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
#'   `bsh_num`, `bat_side`, `pre_balls`, `pre_strikes`, `strike_zone_top`, `strike_zone_bottom`
#' 
#' @return a dataframe of simulated pitch characteristics
#' 
#' @export
#' 
sim_pitches <- function(pitch_distribution_model, pitcher_id, n, context) {

  # Unpack pitch distribution model ----

  league_params <- model$league_params
  pitch_char_vec <- model$pitch_char_vec

  map <- list()   # maximum a posteriori parameter estimates
  parameters <- c("mu", "sigma", "pivar", "nu", "xi", "Rho", "lambda", "zeta", "theta", "kappa")
  for (parameter in parameters) {
    map[[parameter]] <- cmdstanr::as_draws(model$cmdstan_fit$draws(parameter))
  }

  pitcher_coef <- expand.grid(
    pitcher_id = model$pitcher_hand$pitcher_id,
    pitch_char = pitch_char_vec
  ) |>
    tibble::add_column(
      mu = c(map$mu),
      pi = c(map$pi),
      nu = c(map$nu),
      xi = c(map$xi),
      sigma = c(map$sigma)
    )

  count_coef <- expand.grid(
    bsh_num = 1:24,
    pitch_char = pitch_char_vec
  ) |> tibble::add_column(
    lambda = c(map$lambda),
    zeta = c(map$zeta)
  )

  general_coef <- tibble::tibble(
    pitch_char = pitch_char_vec,
    theta = c(map$theta),
    kappa = c(map$kappa)
  )

  all_rho <- array(map$Rho,
    dim = c(nrow(model$pitcher_hand), length(pitch_char_vec), length(pitch_char_vec))
  )
  rho <- all_rho[which(model$pitcher_hand$pitcher_id == pitcher_id), , ]


  # Simulate pitches ----

  simmed_context <- context |>
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
    dplyr::left_join(pitcher_hand, by = "pitcher_id") |>
    dplyr::left_join(pitcher_coef, by = c("pitcher_id", "pitch_char")) |>
    dplyr::left_join(count_coef, by = c("bsh_num", "pitch_char")) |>
    dplyr::left_join(general_coef, by = "pitch_char") |>
    dplyr::mutate(
      same_hand = as.numeric(pitch_hand == bat_side),
      mean = mu +                                               # pitcher intercept
        pi * (same_hand - league_params$same_hand_mean) +       # pitcher slope for same_hand
        nu * (pre_balls - league_params$pre_balls_mean) +       # pitcher slope for pre_balls
        xi * (pre_strikes - league_params$pre_strikes_mean) +   # pitcher slope for pre_strikes
        lambda +                                                # count intercept
        theta * (strike_zone_top - league_params$strike_zone_top_mean) / league_params$strike_zone_top_sd +           # slope for strike_zone_top
        kappa * (strike_zone_bottom - league_params$strike_zone_bottom_mean) / league_params$strike_zone_bottom_sd,   # slope for strike_zone_bottom
      sd = sigma * zeta   # pitcher effect x count effect
    ) |>
    dplyr::mutate(value = mean + sd * z_score) |>
    dplyr::select(sim_num, pitch_char, value) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = value) |>
    # Reverse x-coordinate flipping for LHP
    dplyr::mutate(
      ax = ifelse(pitch_hand == "L", -1, 1) * ax,
      bx = ifelse(pitch_hand == "L", -1, 1) * bx,
      cx = ifelse(pitch_hand == "L", -1, 1) * cx
    )

  return(simmed_pitch)
}
