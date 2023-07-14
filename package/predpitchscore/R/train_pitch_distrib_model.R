#' Estimate pitch distribution model via MAP parameters
#' 
#' We use cmdstanr to estimate MAP parameters for a hierarchical Bayesian model for pitch
#' characteristic means, standard deviations, and covariance matrices.
#' 
#' @param pitch dataframe of pitch data from \code{\link{extract_season}}
#' @param event dataframe of event data from \code{\link{extract_season}}
#' @param pt character, abbreviated pitch type for which to fit model
#' @param pitch_char_vec character vector of pitch characteristics to model
#' 
#' @return a fitted "pitch_distrib_model" object
#' 
#' @export
#' 
train_pitch_distrib_model <- function(pitch,
                                      event,
                                      pt = c("FF", "SI", "SL", "CU", "KC", "CH", "FS", "RC"),
                                      pitch_char_vec = c(
                                       "ax", "bx", "cx", "ay", "by", "cy", "az", "bz", "cz"
                                      )) {

  data <- pitch |>
    dplyr::left_join(event, by = c("year", "game_id", "event_index")) |>
    dplyr::filter(pitch_type == pt, !is.na(extension), sqrt(vx0^2 + vy0^2 + vz0^2) >= 70) |>
    # Filter down to pitchers with at least 100 pitches
    dplyr::group_by(year, pitcher_id) |>
    dplyr::filter(dplyr::n() >= 100) |>
    dplyr::ungroup() |>
    # Get necessary pitch characteristics and context variables
    get_quadratic_coef() |>
    dplyr::mutate(
      pitcher_num = match(pitcher_id, sort(unique(pitcher_id))),  # stan needs pitchers numbered 1:n
      # Flip x-coordinate for LHP
      ax = ifelse(pitch_hand == "L", -1, 1) * ax,
      bx = ifelse(pitch_hand == "L", -1, 1) * bx,
      cx = ifelse(pitch_hand == "L", -1, 1) * cx,
      same_hand = as.numeric(pitch_hand == bat_side),
      bsh_num = same_hand * 12 + pre_balls * 3 + pre_strikes + 1
    )
  
  pitcher_hand <- data |>
    dplyr::group_by(pitcher_id) |>
    dplyr::summarize(pitch_hand = ifelse(mean(pitch_hand == "L") > 0.5, "L", "R"))
  
  # Before standardizing pitch trajectory and context variables, calculate league means and SDs
  league_params <- data |>
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::all_of(
          c(pitch_char_vec,
            "pre_balls", "pre_strikes", "same_hand", "strike_zone_top", "strike_zone_bottom"
          )
        ),
        .fns = list(mean = mean, sd = sd)
      )
    )
  
  # Standardize pitch trajectory and context variables
  data_standardized <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c(pitch_char_vec, "strike_zone_top", "strike_zone_bottom")),
        .fns = ~ (. - mean(.)) / sd(.)
      ),
      dplyr::across(
        .cols = c(pre_balls, pre_strikes, same_hand),
        .fns = ~ . - mean(.)
      )
    )

  pitch_trajectory_coefficients <- data_standardized |>
    dplyr::select(dplyr::all_of(pitch_char_vec))

  bsh_weights <- data_standardized |>
    dplyr::count(bsh_num) |>
    dplyr::mutate(weight = n / sum(n))

  stan_data <- with(data_standardized,
    list(
      n = nrow(data_standardized),
      s = max(pitcher_num),
      c = 9,
      v = pitch_trajectory_coefficients,
      sz_top = strike_zone_top,
      sz_bottom = strike_zone_bottom,
      balls = pre_balls,
      strikes = pre_strikes,
      hand = same_hand,
      bsh = bsh_num,
      p = pitcher_num,
      bsh_weights = bsh_weights$weight
    )
  )
 
  model <- cmdstanr::cmdstan_model(
    stan_file = system.file("stan", "pitch_distrib_model.stan", package = "predpitchscore")
  )
  
  cmdstan_fit <- model$optimize(
    data = stan_data,
    seed = 123,
    iter = 15000,
    algorithm = "lbfgs",
    tol_rel_grad = 1e+3,
    tol_param = 1e-9,
    init = initialize_pitch_posterior()
  )

  model <- list(
    cmdstan_fit = cmdstan_fit,
    pitch_char_vec = pitch_char_vec,
    pitcher_hand = pitcher_hand,
    league_params = league_params
  )

  class(model) <- "pitch_distrib_model"

  return(model)
}




#' Set initial parameter values for pitch distribution model
#' 
#' @param data a dataframe of pitch characteristics and context variables
#' 
#' @return a list of initial values to pass directly to cmdstanr::cmdstan_model
#' 
initialize_pitch_distrib_model <- function(data) {

  inits <- data |>
    dplyr::group_by(pitcher_num) |>
    dplyr::summarize(
      dplyr::across(
        .cols = c(ax, ay, az, bx, by, bz, cx, cy, cz),
        .fns = list(mean = mean, sd = sd)
      )
    )

  ballast <- (1 / apply(as.matrix(inits[, 2:10]), 2, sd)^2) / (1 / colMeans(as.matrix(inits[, 11:19])) * 0.95)^2

  var_prior <- 1 / (colMeans(as.matrix(inits[, 11:19])^{-2}))

  ballast_2 <- 2 * colMeans(as.matrix(inits[,11:19])^2) / (colMeans(as.matrix(inits[, 11:19])^2) - var_prior)

  inits_1.5 <- data |>
    dplyr::group_by(pitcher_num) |>
    dplyr::summarize(
      ax_mean = sum(ax) / (dplyr::n() + ballast[1]), #Was struggling at getting the batter stuff to work well. Took it out for now
      ay_mean = sum(ay) / (dplyr::n() + ballast[2]),
      az_mean = sum(az) / (dplyr::n() + ballast[3]),
      bx_mean = sum(bx) / (dplyr::n() + ballast[4]),
      by_mean = sum(by) / (dplyr::n() + ballast[5]),
      bz_mean = sum(bz) / (dplyr::n() + ballast[6]),
      cx_mean = sum(cx) / (dplyr::n() + ballast[7]),
      cy_mean = sum(cy) / (dplyr::n() + ballast[8]),
      cz_mean = sum(cz) / (dplyr::n() + ballast[9]),
      ax_sd = ((var(ax) * dplyr::n() + var_prior[1] * ballast_2[1]) / (dplyr::n() + ballast_2[1]))^0.5,
      ay_sd = ((var(ay) * dplyr::n() + var_prior[2] * ballast_2[2]) / (dplyr::n() + ballast_2[2]))^0.5,
      az_sd = ((var(az) * dplyr::n() + var_prior[3] * ballast_2[3]) / (dplyr::n() + ballast_2[3]))^0.5,
      bx_sd = ((var(bx) * dplyr::n() + var_prior[4] * ballast_2[4]) / (dplyr::n() + ballast_2[4]))^0.5,
      by_sd = ((var(by) * dplyr::n() + var_prior[5] * ballast_2[5]) / (dplyr::n() + ballast_2[5]))^0.5,
      bz_sd = ((var(bz) * dplyr::n() + var_prior[6] * ballast_2[6]) / (dplyr::n() + ballast_2[6]))^0.5,
      cx_sd = ((var(cx) * dplyr::n() + var_prior[7] * ballast_2[7]) / (dplyr::n() + ballast_2[7]))^0.5,
      cy_sd = ((var(cy) * dplyr::n() + var_prior[8] * ballast_2[8]) / (dplyr::n() + ballast_2[8]))^0.5,
      cz_sd = ((var(cz) * dplyr::n() + var_prior[9] * ballast_2[9]) / (dplyr::n() + ballast_2[9]))^0.5
    )

  inits_2 <- data |>
    dplyr::group_by(bsh_num) |>
    dplyr::summarize(
      ax_mean = mean(ax),
      ay_mean = mean(ay),
      az_mean = mean(az),
      bx_mean = mean(bx),
      by_mean = mean(by),
      bz_mean = mean(bz),
      cx_mean = mean(cx),
      cy_mean = mean(cy),
      cz_mean = mean(cz),
      ax_sd = tidyr::replace_na(sd(ax), 1),
      ay_sd = tidyr::replace_na(sd(ay), 1),
      az_sd = tidyr::replace_na(sd(az), 1),
      bx_sd = tidyr::replace_na(sd(bx), 1),
      by_sd = tidyr::replace_na(sd(by), 1),
      bz_sd = tidyr::replace_na(sd(bz), 1),
      cx_sd = tidyr::replace_na(sd(cx), 1),
      cy_sd = tidyr::replace_na(sd(cy), 1),
      cz_sd = tidyr::replace_na(sd(cz), 1)
    )

  inits_5 <- data |>
    dplyr::group_by(pitcher_num) |>
    dplyr::summarize(
      axnu = cov(ax, pre_balls) / (0.001 + var(pre_balls)),
      aynu = cov(ay, pre_balls) / (0.001 + var(pre_balls)),
      aznu = cov(az, pre_balls) / (0.001 + var(pre_balls)),
      bxnu = cov(bx, pre_balls) / (0.001 + var(pre_balls)),
      bynu = cov(by, pre_balls) / (0.001 + var(pre_balls)),
      bznu = cov(bz, pre_balls) / (0.001 + var(pre_balls)),
      cxnu = cov(cx, pre_balls) / (0.001 + var(pre_balls)),
      cynu = cov(cy, pre_balls) / (0.001 + var(pre_balls)),
      cznu = cov(cz, pre_balls) / (0.001 + var(pre_balls)),
      axxi = cov(ax, pre_strikes)/ (0.001 + var(pre_strikes)),
      ayxi = cov(ay, pre_strikes)/ (0.001 + var(pre_strikes)),
      azxi = cov(az, pre_strikes)/ (0.001 + var(pre_strikes)),
      bxxi = cov(bx, pre_strikes)/ (0.001 + var(pre_strikes)),
      byxi = cov(by, pre_strikes)/ (0.001 + var(pre_strikes)),
      bzxi = cov(bz, pre_strikes)/ (0.001 + var(pre_strikes)),
      cxxi = cov(cx, pre_strikes)/ (0.001 + var(pre_strikes)),
      cyxi = cov(cy, pre_strikes)/ (0.001 + var(pre_strikes)),
      czxi = cov(cz, pre_strikes)/ (0.001 + var(pre_strikes)),
      axpi = cov(ax, same_hand) / (0.001 + var(same_hand)),
      aypi = cov(ay, same_hand) / (0.001 + var(same_hand)),
      azpi = cov(az, same_hand) / (0.001 + var(same_hand)),
      bxpi = cov(bx, same_hand) / (0.001 + var(same_hand)),
      bypi = cov(by, same_hand) / (0.001 + var(same_hand)),
      bzpi = cov(bz, same_hand) / (0.001 + var(same_hand)),
      cxpi = cov(cx, same_hand) / (0.001 + var(same_hand)),
      cypi = cov(cy, same_hand) / (0.001 + var(same_hand)),
      czpi = cov(cz, same_hand) / (0.001 + var(same_hand))
    )

  inits_5 <- inits_5 - rep(colMeans(inits_5), each = stan_data$s)

  z_score_init <- data |>
    dplyr::select(pitcher_num, ax, bx, cx, ay, by, cy, cz, az, bz, cz) |>
    dplyr::left_join(inits_1.5, by = "pitcher_num") |>
    dplyr::transmute(
      ax_z = (ax - 0.97 * ax_mean) / (0.9 * ax_sd + 0.1),
      bx_z = (bx - 0.97 * bx_mean) / (0.9 * bx_sd + 0.1),
      cx_z = (cx - 0.97 * cx_mean) / (0.9 * cx_sd + 0.1),
      ay_z = (ay - 0.97 * ay_mean) / (0.9 * ay_sd + 0.1),
      by_z = (by - 0.97 * by_mean) / (0.9 * by_sd + 0.1),
      cy_z = (cy - 0.97 * cy_mean) / (0.9 * cy_sd + 0.1),
      az_z = (az - 0.97 * az_mean) / (0.9 * az_sd + 0.1),
      bz_z = (bz - 0.97 * bz_mean) / (0.9 * bz_sd + 0.1),
      cz_z = (cz - 0.97 * cz_mean) / (0.9 * cz_sd + 0.1)
    )
  
  league_Rho_init <- array(rep(t(chol(cor(z_score_init))), each = stan_data$s), dim = c(stan_data$s, 9, 9))
  player_Rho_init <- array(rep(t(chol(cor(z_score_init))), each = stan_data$s), dim = c(stan_data$s, 9, 9))
  Rho_init <- array(rep(t(chol(cor(z_score_init))), each = stan_data$s), dim = c(stan_data$s, 9, 9))
  league_corr <- cor(z_score_init)

  for(i in 1:stan_data$s) {

    player_data <- data |>
      dplyr::filter(pitcher_num == i)
    player_corr <- player_data |>
      dplyr::select(ax, bx, cx, ay, by, cy, az, bz, cz) |>
      cor()
    player_Rho_init[i, , ] = t(chol(player_corr))


    weight <- sqrt(nrow(player_data)) / (sqrt(nrow(player_data)) + 7)
    Rho_init[i, , ] = t(chol(player_corr * weight + league_corr * (1-weight)))
  }
  
  mod <- cmdstanr::cmdstan_model(file.path("inst", "stan", "distribution_model.stan"))
  
  init <- list(
    list(
      gamma = inits |>
        dplyr::select(dplyr::ends_with("_mean")) |>
        apply(2, sd),
      epsilon = inits |>
        dplyr::select(dplyr::ends_with("_sd")) |>
        apply(2, mean) *
        0.98,
      tau = inits |>
        dplyr::select(dplyr::ends_with("_mean")) |>
        apply(2, mean) *
        0.97,
      eta = inits |>
        dplyr::select(dplyr::ends_with("_sd")) |>
        apply(2, sd) *
        0.86,
      sigma = inits_1.5 |>
        dplyr::select(dplyr::ends_with("_sd")) *
        0.95,
      mu = inits_1.5 |>
        dplyr::select(dplyr::ends_with("_mean")),
      leagueRho = t(chol(cor(z_score_init))),
      Rho = Rho_init,
      lambdanorm = inits_2 |>
        dplyr::select(dplyr::ends_with("_mean")) |>
        dplyr::slice(2:24) *
        0.82 /
        0.2,
      zetanorm = inits_2 |>
        dplyr::select(dplyr::ends_with("_sd")) |>
        dplyr::slice(2:24) /
        0.1 -
        10,
      theta = rep(0, stan_data$c),
      kappa = rep(0, stan_data$c),
      nunorm = 0.5 * inits_5[2:stan_data$s, 2:10] / 0.05,
      xinorm = 0.5 * inits_5[2:stan_data$s, 11:19] / 0.05,
      pinorm = 0.6 * inits_5[2:stan_data$s, 20:28] / 0.1
    )
  )

  return(init)
}
