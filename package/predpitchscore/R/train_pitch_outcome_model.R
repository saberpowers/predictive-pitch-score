#' Train pitch outcome model
#' 
#' Train models to predict the outcome tree of a pitch: probability of swing; probability of hbp
#' given no swing; probability of strike given no swing and no hbp; probability of contact given
#' swing; and probability of fair given contact.
#' 
#' @param pitch dataframe of pitch data from \code{\link{extract_season}}
#' @param count_value dataframe of count_value from \code{\link{compute_count_value}},
#'   not used but stashed in the model object for pitch value prediction
#' @param components character vector specifying which component models to fit
#' @param stuff_only logical, fit model with only "Stuff" features (no pitch location)?
#' @param tune logical, if true, tune the hyperparameters instead of fitting the model
#' 
#' @return a fitted "pitch_outcome_model" object
#' 
#' @export
#' 
train_pitch_outcome_model <- function(pitch,
                                      count_value,
                                      components = c("swing", "hbp", "strike", "contact", "fair", "hit", "value"),
                                      stuff_only = FALSE,
                                      tune = FALSE) {

  # Wrangle training data ----
  
  outcome_model_features <- pitch |>
    get_quadratic_coef() |>
    get_outcome_model_features()
  outcome_tree <- get_outcome_tree(pitch$description)

  trackman_metrics <- pitch |>
    get_quadratic_coef() |>
    get_trackman_metrics() |>
    dplyr::select(release_speed, horz_break, induced_vert_break)
  
  regression_data <- pitch |>
    # Make sure we don't duplicate columns
    dplyr::select(pre_balls, pre_strikes, is_rhb, strike_zone_top, strike_zone_bottom, hit_pred, true_value, x0, z0) |>
    dplyr::bind_cols(outcome_model_features, outcome_tree, trackman_metrics) |>
    dplyr::filter(!is.na(extension))
  

  # Train xgboost models ----

  if (stuff_only) {
    features <- with(config_pitch_outcome_xgb, c(context_features, stuff_features))
  } else {
    features <- with(config_pitch_outcome_xgb, c(context_features, pitch_features))
  }

  if ("swing" %in% components) {
    xgb_swing <- regression_data |>
      dplyr::mutate(label = is_swing) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "swing")
  } else {
    xgb_swing <- NULL
  }
  
  if ("hbp" %in% components) {
    xgb_hbp <- regression_data |>
      dplyr::filter(!is_swing) |>
      dplyr::mutate(label = is_hbp) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "hbp")
  } else {
    xgb_hbp <- NULL
  }

  if ("strike" %in% components) {
    xgb_strike <- regression_data |>
      dplyr::filter(!is_swing, !is_hbp) |>
      dplyr::mutate(label = is_strike) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "strike")
  } else {
    xgb_strike <- NULL
  }
  
  if ("contact" %in% components) {
    xgb_contact <- regression_data |>
      dplyr::filter(is_swing) |>
      dplyr::mutate(label = is_contact) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "contact")
  } else {
    xgb_contact <- NULL
  }
  
  if ("fair" %in% components) {
    xgb_fair <- regression_data |>
      dplyr::filter(is_swing, is_contact) |>
      dplyr::mutate(label = is_fair) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "fair")
  } else {
    xgb_fair <- NULL
  }
  
  if ("hit" %in% components) {
    xgb_hit <- regression_data |>
      dplyr::filter(is_swing, is_contact, is_fair, !is.na(hit_pred)) |>
      dplyr::mutate(label = hit_pred) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "hit")
  } else {
    xgb_hit <- NULL
  }

  if ("value" %in% components) {
    xgb_value <- regression_data |>
      dplyr::filter(!is.na(true_value)) |>
      dplyr::mutate(label = true_value) |>
      train_pitch_outcome_xgb(features = features, tune = tune, label = "value")
  } else {
    xgb_value <- NULL
  }


  # Combine models and return ----
  
  xgb <- list(
    swing = xgb_swing,
    hbp = xgb_hbp,
    strike = xgb_strike,
    contact = xgb_contact,
    fair = xgb_fair,
    hit = xgb_hit,
    value = xgb_value
  )

  model <- list(xgb = xgb, count_value = count_value, features = features)
  
  class(model) <- "pitch_outcome_model"

  return(model)  
}




#' Configuration for pitch outcome XGBoost models
#' 
#' Use this object to specify which features to include in the pitch outcome model and which values
#' to use for each tuning parameter. We have a separate script for determining optimal parameters.
#' 
config_pitch_outcome_xgb <- list(

  context_features = c("pre_balls", "pre_strikes", "is_rhb", "strike_zone_top", "strike_zone_bottom"),
  pitch_features = c("plate_x", "plate_z", "plate_vx", "plate_vy", "plate_vz", "ax", "ay", "az", "extension"),
  stuff_features = c("horz_break","induced_vert_break","release_speed","z0", "ay", "x0", "extension"),

  nrounds_swing = 2000,
  params_swing = list(eta = 0.05, gamma = 0, max_depth = 9, min_child_weight = 10, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_hbp = 1000,
  params_hbp = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_strike = 1000,
  params_strike = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_contact = 1000,
  params_contact = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_fair = 1500,
  params_fair = list(eta = 0.01, gamma = 0, max_depth = 9, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_hit = 1000,
  params_hit = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  nrounds_value = 2000,
  params_value = list(eta = 0.01, gamma = 0, max_depth = 6, min_child_weight = 100, subsample = 0.65, colsample_bytree = 0.7),

  # Maximum nrounds for tuning
  nrounds_max = 2000,

  # List of parameter combinations to try for tuning
  params_list = expand.grid(
    nthread = 1,  # turn off xgb threading (threading across parameter sets is more efficient)
    eta = c(0.01, 0.05, 0.3),
    gamma = 0,
    max_depth = c(3, 6, 9),
    min_child_weight = c(10, 30, 100),
    subsample = 0.65,
    colsample_bytree = 0.7
  ) |>
    apply(MARGIN = 1, FUN = as.list)
)




#' Train a pitch outcome sub-model using xgboost
#' 
#' This function is a wrapper for xgboost to be used for training the sub-components of the pitch
#' outcome model.
#' 
#' @param data_subset a dataframe of pitch data subsetted to pitches of interest
#' @param features a character vector of column names from `data_subset` to be used as features
#' @param label a character string, one of "swing", "hbp", "strike", "contact", "fair", "hit",
#'   used to determine which tuning parmeters to use from \code{\link{config_pitch_outcome_xgb}}
#' @param tune logical, if true, tune the hyperparameters instead of fitting the model
#' @param verbose argument passed directly to `xgboost::xgboost`, defaults to silent
#' @param ... additional parameters to pass to `xgboost::xgboost`
#' 
#' @return a fitted `xgboost::xgboost` model
#' 
train_pitch_outcome_xgb <- function(data_subset,
                                    features,
                                    tune,
                                    label = c("swing", "hbp", "strike", "contact", "fair", "hit", "value"),
                                    verbose = 0,
                                    ...) {

  label <- match.arg(label)

  response <- ifelse(label %in% c("hit", "value"), "gaussian", "binomial")

  covariate_matrix <- data_subset |>
    dplyr::select(dplyr::all_of(features)) |>
    as.matrix()
  
  if (tune) {

    cv_result <- tune_xgb_parallel(
      covariate_matrix = covariate_matrix,
      label = data_subset$label,
      params_list = config_pitch_outcome_xgb$params_list,
      nrounds = config_pitch_outcome_xgb$nrounds_max,
      response = response
    )

    test_error_column <- dplyr::case_when(
      response == "gaussian" ~ "test_rmse_mean",
      response == "binomial" ~ "test_logloss_mean"
    )

    # Extract best-fit parameters
    cv_result_best <- cv_result |>
      dplyr::arrange(!!rlang::sym(test_error_column)) |>
      dplyr::slice(1)
      
    params <- cv_result_best |>
      dplyr::select(
        objective, eta, gamma, max_depth, min_child_weight, subsample, colsample_bytree
      ) |>
      as.list()

    nrounds <- cv_result_best$iter

  } else {
    params <- config_pitch_outcome_xgb[[glue::glue("params_{label}")]]
    nrounds <- config_pitch_outcome_xgb[[glue::glue("nrounds_{label}")]]
  }

  if (label %in% c("hit", "value")) {
    response <- "gaussian"
    params$objective <- "reg:squarederror"
  } else {
    response <- "binomial"
    params$objective <- "binary:logistic"
  }

  model <- xgboost::xgb.train(
    params = params,
    data = xgboost::xgb.DMatrix(data = covariate_matrix, label = data_subset$label),
    nrounds = nrounds,
    verbose = verbose
  )

  if (tune) {
    model$cv_result <- cv_result
  }

  return(model)
}




#' Extract predictions from a fitted pitch outcome model
#' 
#' @param object a fitted "pitch_outcome_model" object
#' @param newpitch a dataframe of pitch data for which to make predictions
#' 
#' @return a dataframe of predictions with columns swing, hbp, strike, contact and fair
#' 
#' @export
#' 
predict.pitch_outcome_model <- function(object, newpitch, ...) {

  outcome_model_features <- newpitch |>
    get_quadratic_coef() |>
    get_outcome_model_features()

  trackman_metrics <- newpitch |>
    get_quadratic_coef() |>
    get_trackman_metrics() |>
    dplyr::select(release_speed, horz_break, induced_vert_break)

  newdata <- newpitch |>
    # Make sure we don't duplicate columns
    dplyr::select(pre_balls, pre_strikes, is_rhb, strike_zone_top, strike_zone_bottom, x0, z0) |>
    dplyr::bind_cols(outcome_model_features, trackman_metrics) |>
    dplyr::select(dplyr::all_of(object$features)) |>
    as.matrix()

  pitch_pred <- tibble::tibble(
    pre_balls = newpitch$pre_balls,
    pre_strikes = newpitch$pre_strikes,
    is_rhb = newpitch$is_rhb,
    strike_zone_top = newpitch$strike_zone_top,
    strike_zone_bottom = newpitch$strike_zone_bottom,
    prob_swing = xgboost:::predict.xgb.Booster(object$xgb$swing,
      newdata = newdata[,colnames(newdata) %in% object$xgb$swing$feature_names, drop = FALSE]
    ),
    prob_hbp = xgboost:::predict.xgb.Booster(object$xgb$hbp,
      newdata = newdata[,colnames(newdata) %in% object$xgb$hbp$feature_names, drop = FALSE]
    ),
    prob_strike = xgboost:::predict.xgb.Booster(object$xgb$strike,
      newdata[,colnames(newdata) %in% object$xgb$strike$feature_names, drop = FALSE]
    ),
    prob_contact = xgboost:::predict.xgb.Booster(object$xgb$contact,
      newdata[,colnames(newdata) %in% object$xgb$contact$feature_names, drop = FALSE]
    ),
    prob_fair = xgboost:::predict.xgb.Booster(object$xgb$fair,
      newdata = newdata[,colnames(newdata) %in% object$xgb$fair$feature_names, drop = FALSE]
    ),
    pred_hit = xgboost:::predict.xgb.Booster(object$xgb$hit,
      newdata = newdata[,colnames(newdata) %in% object$xgb$hit$feature_names, drop = FALSE]
    ),
    pred_value = xgboost:::predict.xgb.Booster(object$xgb$value,
      newdata = newdata[,colnames(newdata) %in% object$xgb$value$feature_names, drop = FALSE]
    )
  )

  pitch_value <- compute_pitch_value(pitch_pred = pitch_pred, count_value = object$count_value)

  pred <- dplyr::bind_cols(pitch_pred, pitch_value) |>
    dplyr::select(
      dplyr::starts_with("prob"),
      dplyr::starts_with("pred"),
      dplyr::starts_with("pitch_value")
    )

  return(pred)
}
