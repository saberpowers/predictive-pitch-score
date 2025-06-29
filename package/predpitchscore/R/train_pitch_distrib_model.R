#' Estimate pitch distribution model via MAP parameters
#' 
#' We use cmdstanr to estimate MAP parameters for a hierarchical Bayesian model for pitch
#' characteristic means, standard deviations, and covariance matrices.
#' 
#' @param data dataframe of pitch and event data
#' @param version character string, "complete" or "conditional". The complete model assumes league
#'   hyperparameters are unknown (takes longer), whereas the conditional model estimates pitcher
#'   parameters conditional on previously estimated league hyperparameters.
#' @param complete_model a fitted "pitch_distrib_model" object required if version == "conditional",
#'   otherwise ignored
#' @param pitch_char_vec character vector of pitch characteristics to model
#' @param ... additional arguments for for cmdstanr optimize method
#' 
#' @return a fitted "pitch_distrib_model" object
#' 
#' @export
#' 
train_pitch_distrib_model <- function(data,
                                      version = c("complete", "conditional"),
                                      complete_model = NULL,
                                      pitch_char_vec = c(
                                       "ax", "bx", "cx", "ay", "by", "cy", "az", "bz", "cz"
                                      ),
                                      ...
                                      ) {

  version <- match.arg(version)

  model_data <- data |>
    dplyr::filter(
      !is.na(extension),
      sqrt(vx0^2 + vy0^2 + vz0^2) >= 70,
      balls < 4,
      strikes < 3
    ) |>
    # Filter out pitchers with insufficient pitch counts
    dplyr::group_by(year, pitcher_id) |>
    dplyr::filter(dplyr::n() >= ifelse(version == "complete", 100, 10)) |>
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
      bsh_num = same_hand * 12 + balls * 3 + strikes + 1
    )
  
  # Record the hand of each pitcher to be used later when producing simulations
  pitcher_hand <- model_data |>
    dplyr::group_by(pitcher_id) |>
    dplyr::summarize(pitch_hand = ifelse(mean(pitch_hand == "L") > 0.5, "L", "R"))
  
  # Record league means and SDs to be used later when producing simulations
  league_params <- model_data |>
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::all_of(
          c(pitch_char_vec,
            "balls", "strikes", "same_hand", "strike_zone_top", "strike_zone_bottom"
          )
        ),
        .fns = list(mean = mean, sd = sd)
      )
    )
  
  # Standardize pitch trajectory and context variables
  data_standardized <- model_data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c(pitch_char_vec, "strike_zone_top", "strike_zone_bottom")),
        .fns = ~ (. - mean(.)) / sd(.)
      ),
      dplyr::across(
        .cols = c(balls, strikes, same_hand),
        .fns = ~ . - mean(.)
      )
    )

  pitch_trajectory_coefficients <- data_standardized |>
    dplyr::select(dplyr::all_of(pitch_char_vec))

  bsh_weights <- data_standardized |>
    dplyr::count(bsh_num) |>
    dplyr::mutate(weight = n / sum(n))

  # Fill in any missing bsh_weights
  full_bsh_weights <- bsh_weights
  for(i in 1:24) {
    row <- bsh_weights |>
      dplyr::filter(bsh_num == i)
    if (length(row$bsh_num) == 1) {
      full_bsh_weights[i, ] <- row
    } else {
      full_bsh_weights[i, 1] <- i
      full_bsh_weights[i, 2:3] <- 0
    }
  }
  bsh_weights <- full_bsh_weights

  stan_data <- with(data_standardized,
    list(
      n = nrow(data_standardized),
      s = nrow(pitcher_hand),
      c = length(pitch_char_vec),
      v = pitch_trajectory_coefficients,
      sz_top = strike_zone_top,
      sz_bottom = strike_zone_bottom,
      balls = balls,
      strikes = strikes,
      hand = same_hand,
      bsh = bsh_num,
      p = pitcher_num,
      bsh_weights = bsh_weights$weight
    )
  )

  # If we're fitting the conditional model, extract league hyperparameters
  # from previously estimated complete model
  if (version == "conditional") {

    league_hyperparameters <- c("tau", "gamma", "epsilon", "eta", "lambda", "zeta", "leagueRho")

    for (parameter in league_hyperparameters) {

      name <- glue::glue("{parameter}_fixed")
      stan_data[[name]] <- as.numeric(complete_model$map[[parameter]])

      # Convert matrix hyperparameters from vector to matrix
      if (parameter %in% c("lambda", "zeta", "leagueRho")) {
        stan_data[[name]] <- matrix(stan_data[[name]], ncol = length(pitch_char_vec))
      }
    }

    stan_data$leagueRho_fixed <- stan_data$leagueRho_fixed /
      sqrt(rowSums(stan_data$leagueRho_fixed^2))
  }

  model <- cmdstanr::cmdstan_model(
    stan_file = system.file(
      "stan", glue::glue("pitch_distrib_model_{version}.stan"),
      package = "predpitchscore"
    )
  )

  cmdstan_init <- initialize_pitch_distrib_model(
    data_standardized = data_standardized,
    pitch_char_vec = pitch_char_vec
  )
  
  cmdstan_fit <- model$optimize(
    data = stan_data,
    seed = 123,
    algorithm = "lbfgs",
    init = cmdstan_init,
    ...
  )

  # Extract MAP parameter estimates from the cmdstanr object because it can be temperamental
  map <- list()   # maximum a posteriori parameter estimates
  for (parameter in cmdstan_fit$metadata()$stan_variables) {
    map[[parameter]] <- cmdstan_fit$draws(parameter)
  }

  model <- list(
    map = map,
    pitch_char_vec = pitch_char_vec,
    pitcher_hand = pitcher_hand,
    league_params = league_params
  )

  class(model) <- "pitch_distrib_model"

  return(model)
}




#' Set initial parameter values for pitch distribution model
#' 
#' @param data_standardized a dataframe of standardized pitch characteristics and context variables
#' @param pitch_char_vec character vector of pitch characteristics to model
#' 
#' @return a list of initial values to pass directly to cmdstanr::cmdstan_model
#' 
initialize_pitch_distrib_model <- function(data_standardized, pitch_char_vec) {

  data_standardized_long <- data_standardized |>
    dplyr::select(
      play_id, pitcher_num, bsh_num, balls, strikes, same_hand,
      dplyr::all_of(pitch_char_vec)
    ) |>
    tidyr::pivot_longer(cols = dplyr::all_of(pitch_char_vec), names_to = "pitch_char")
  
  pitcher_params <- data_standardized_long |>
    dplyr::group_by(pitcher_num, pitch_char) |>
    dplyr::summarize(
      n = dplyr::n(),
      mean = mean(value),
      sd = tidyr::replace_na(sd(value), 0.5),
      nu = tidyr::replace_na(cov(value, balls) / (0.001 + var(balls)), 0),
      xi = tidyr::replace_na(cov(value, strikes)/ (0.001 + var(strikes)), 0),
      pi = tidyr::replace_na(cov(value, same_hand) / (0.001 + var(same_hand)), 0),
      .groups = "drop"
    ) |>
    dplyr::group_by(pitch_char) |>
    dplyr::mutate(
      nu = nu - mean(nu),
      xi = xi - mean(xi),
      pi = pi - mean(pi),
      ballast = (1 / var(mean)) / (1 / mean(sd) * 0.95)^2,
      var_prior = 1 / mean(sd^{-2}),
      ballast_2 = 2 * mean(sd^2) / (mean(sd^2) - var_prior)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      mean_adj = mean * n / (n + ballast),
      sd_adj = sqrt((sd^2 * n + var_prior * ballast_2) / (n + ballast_2))
    )
  
  pitch_char_summary <- pitcher_params |>
    dplyr::group_by(pitch_char) |>
    dplyr::summarize(
      gamma = sd(mean),
      epsilon = 0.98 * mean(sd),
      tau = 0.97 * mean(mean),
      eta = 0.86 * sd(sd)
    )
  
  pitcher_mean_adj <- pitcher_params |>
    dplyr::select(pitcher_num, pitch_char, mean_adj) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = mean_adj) |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    as.matrix()

  pitcher_sd_adj <- pitcher_params |>
    dplyr::select(pitcher_num, pitch_char, sd_adj) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = sd_adj) |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    as.matrix()

  pitcher_nu <- pitcher_params |>
    dplyr::select(pitcher_num, pitch_char, nu) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = nu) |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    as.matrix()

  pitcher_xi <- pitcher_params |>
    dplyr::select(pitcher_num, pitch_char, xi) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = xi) |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    as.matrix()

  pitcher_pi <- pitcher_params |>
    dplyr::select(pitcher_num, pitch_char, pi) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = pi) |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    as.matrix()

  bsh_params_raw <- data_standardized |>
    dplyr::group_by(bsh_num) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(pitch_char_vec), list(mean = mean, sd = sd, n = length)),
      .groups = "drop"
    )

  # It's possible that some *rows* are missing from bsh_params, so we need to identify those
  # missing rows and replace them with placeholder values
  bsh_params <- bsh_params_raw |>
    dplyr::mutate(
      bsh_num = 1:dplyr::n(),
      dplyr::across(dplyr::ends_with("_n"), \(x) 1),
      dplyr::across(dplyr::ends_with("_mean"), \(x) 0),
      dplyr::across(dplyr::ends_with("_sd"), \(x) 1)
    )
  bsh_params[bsh_params$bsh_num, ] <- bsh_params_raw
  
  bsh_ns <- bsh_params |>
    dplyr::select(dplyr::ends_with("_n")) |>
    as.matrix() |>
    tidyr::replace_na(1)
  
  bsh_means <- bsh_params |>
    dplyr::select(dplyr::ends_with("_mean")) |>
    as.matrix()
  
  bsh_sds <- bsh_params |>
    dplyr::select(dplyr::ends_with("_sd")) |>
    as.matrix() |>
    tidyr::replace_na(1)

  # Apply shrinkage to estimated standard deviations
  # We learned that for subsets of data with tiny samples (e.g. 3-0 curveballs vs RHBs), wacky
  # estimates of the standard deviations can result in poor initializations of zeta.
  # In what follows, we use empirical bayes to shrink the estimated standard deviations, assuming
  # a Normal(1, empirical variance across bsh) prior and a Normal(true value, (n-1)/2) likelihood,
  # which is based on the chi-square sampling distribution of the sample variance.
  # https://github.com/saberpowers/predictive-pitch-score/issues/72
  bsh_sds_var <- matrix(
    data = apply(bsh_sds, 2, var),
    nrow = nrow(bsh_sds),
    ncol = ncol(bsh_sds),
    byrow = TRUE
  )
  bsh_sds_shrunk <- (1 / bsh_sds_var + (bsh_ns - 1) / 2 * bsh_sds) / (1 / bsh_sds_var + (bsh_ns - 1) / 2)

  z_scores <- data_standardized_long |>
    dplyr::left_join(pitcher_params, by = c("pitcher_num", "pitch_char")) |>
    dplyr::transmute(
      play_id,
      pitcher_num,
      pitch_char,
      z = (value - 0.97 * mean_adj) / (0.9 * sd_adj + 0.1)
    ) |>
    tidyr::pivot_wider(names_from = pitch_char, values_from = z)

  league_corr <- z_scores |>
    dplyr::select(dplyr::all_of(pitch_char_vec)) |>
    cor()

  Rho_init <- array(NA,
    dim = c(max(data_standardized$pitcher_num), length(pitch_char_vec), length(pitch_char_vec))
  )

  for (num in 1:max(data_standardized$pitcher_num)) {

    player_data <- z_scores |>
      dplyr::filter(pitcher_num == num) |>
      dplyr::select(dplyr::all_of(pitch_char_vec))
      
    player_corr <- cor(player_data)

    weight <- sqrt(nrow(player_data) - 1) / (sqrt(nrow(player_data)) + 7)

    Rho_init[num, , ] <- t(chol(weight * player_corr + (1 - weight) * league_corr))
  }

  init <- list(
    list(
      gamma = pitch_char_summary$gamma,
      epsilon = pitch_char_summary$epsilon,
      tau = pitch_char_summary$tau,
      eta = pitch_char_summary$eta,
      sigma = 0.95 * pitcher_sd_adj,
      mu = 0.95 * pitcher_mean_adj,
      leagueRho = t(chol(league_corr)),
      Rho = Rho_init,
      lambdanorm = bsh_means[-1, ] * 0.82 / 0.2,
      zetanorm = (bsh_sds_shrunk[-1, ] - 1) / 0.1,
      theta = rep(0, length(pitch_char_vec)),
      kappa = rep(0, length(pitch_char_vec)),
      nunorm = 0.5 * pitcher_nu[-1, ] / 0.05,
      xinorm = 0.5 * pitcher_xi[-1, ] / 0.05,
      pinorm = 0.6 * pitcher_pi[-1, ] / 0.1
    )
  )

  return(init)
}
