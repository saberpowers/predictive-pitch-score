#' Train pitch outcome model
#' 
#' Train models to predict the outcome tree of a pitch: probability of swing; probability of hbp
#' given no swing; probability of strike given no swing and no hbp; probability of contact given
#' swing; and probability of fair given contact.
#' 
#' @param data a dataframe of pitch data with which to train the pitch outcome model
#' 
#' @return a fitted "pitch_outcome_model" object
#' 
train_pitch_outcome_model <- function(data) {

  # Establish xgboost parameters ----
  # TODO: These should be treated as config

  features <- c("balls_start", "strikes_start", "plate_x", "plate_z",
    "plate_vx", "plate_vy", "plate_vz", "ax", "ay", "az", "extension"
  )
  xgb_nrounds <- 10 # temporarily set to 10 for testing (150 will be better)
  xgb_params <- list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic")


  # Wrangle training data ----
  
  outcome_model_features <- data |>
    get_quadratic_coef() |>
    get_outcome_model_features()
  outcome_tree <- get_outcome_tree(data$description)
  
  regression_data <- data |>
    dplyr::select(balls_start, strikes_start) |>
    dplyr::bind_cols(outcome_model_features, outcome_tree) |>
    dplyr::filter(!is.na(extension))
  

  # Train xgboost models ----

  model_swing <- regression_data |>
    dplyr::mutate(label = is_swing) |>
    train_pitch_outcome_xgb(features = features, nrounds = xgb_nrounds, params = xgb_params)
  
  model_hbp <- regression_data |>
    dplyr::filter(!is_swing) |>
    dplyr::mutate(label = is_hbp) |>
    train_pitch_outcome_xgb(features = features, nrounds = xgb_nrounds, params = xgb_params)
  
  model_strike <- regression_data |>
    dplyr::filter(!is_swing, !is_hbp) |>
    dplyr::mutate(label = is_strike) |>
    train_pitch_outcome_xgb(features = features, nrounds = xgb_nrounds, params = xgb_params)
  
  model_contact <- regression_data |>
    dplyr::filter(is_swing) |>
    dplyr::mutate(label = is_contact) |>
    train_pitch_outcome_xgb(features = features, nrounds = xgb_nrounds, params = xgb_params)
  
  model_fair <- regression_data |>
    dplyr::filter(is_swing, is_contact) |>
    dplyr::mutate(label = is_fair) |>
    train_pitch_outcome_xgb(features = features, nrounds = xgb_nrounds, params = xgb_params)


  # Combine models and return ----
  
  model <- list(
    swing = model_swing,
    hbp = model_hbp,
    strike = model_strike,
    contact = model_contact,
    fair = model_fair
  )
  
  class(model) <- "pitch_outcome_model"

  return(model)  
}




#' Train a pitch outcome sub-model using xgboost
#' 
#' This function is a wrapper for xgboost to be used for training the sub-components of the pitch
#' outcome model.
#' 
#' @param data_subset a dataframe of pitch data subsetted to pitches of interest
#' @param features a character vector of column names from `data_subset` to be used as features
#' @param verbose argument passed directly to `xgboost::xgboost`, defaults to silent
#' @param ... additional parameters to pass to `xgboost::xgboost`
#' 
#' @return
#' 
train_pitch_outcome_xgb <- function(data_subset, features, verbose = 0, ...) {

  model <- xgboost::xgboost(
    data = data_subset |>
      dplyr::select(dplyr::all_of(features)) |>
      as.matrix(),
    label = data_subset$label,
    verbose = verbose,
    ...
  )

  return(model)
}




#' Extract predictions from a fitted pitch outcome model
#' 
#' @param object a fitted "pitch_outcome_model" object
#' @param newdata a dataframe of pitch data for which to make predictions
#' 
#' @return a dataframe of predictions with columns swing, hbp, strike, contact and fair
#' 
predict.pitch_outcome_model <- function(object, newdata, ...) {

  pred <- tibble::tibble()

  pred$swing <- predict(object$swing, newdata = newdata)
  pred$hbp <- predict(object$hbp, newdata = newdata)
  pred$strike <- predict(object$strike, newdata = newdata)
  pred$contact <- predict(object$contact, newdata = newdata)
  pred$fair <- predict(object$fair, newdata = newdata)

  return(pred)
}
