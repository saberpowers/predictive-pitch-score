#' Train pitch outcome model
#' 
#' Train models to predict the outcome tree of a pitch: probability of swing; probability of hbp
#' given no swing; probability of strike given no swing and no hbp; probability of contact given
#' swing; and probability of fair given contact.
#' 
#' @param pitch dataframe of pitch data from \code{\link{extract_season}}
#' @param count_value dataframe of count_value from \code{\link{compute_count_value}},
#'   not used but stashed in the model object for pitch value prediction
#' 
#' @return a fitted "pitch_outcome_model" object
#' 
#' @export
#' 
train_pitch_outcome_model <- function(pitch, count_value, stuff) {

  # Wrangle training data ----
  
  outcome_model_features <- pitch |>
    get_quadratic_coef() |>
    get_outcome_model_features()
  outcome_tree <- get_outcome_tree(pitch$description)
  
  regression_data <- pitch |>
    dplyr::select(pre_balls, pre_strikes, RHB, strike_zone_top, strike_zone_bottom, hit_pred) |>    # make sure we don't duplicate columns
    dplyr::bind_cols(outcome_model_features, outcome_tree) |>
    dplyr::filter(!is.na(extension))
  

  # Train xgboost models ----
  if(stuff==TRUE){
  xgb_swing <- regression_data |>
    dplyr::mutate(label = is_swing) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "swing")
  
  xgb_hbp <- regression_data |>
    dplyr::filter(!is_swing) |>
    dplyr::mutate(label = is_hbp) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "hbp")
 }
  else {
    xgb_swing <- regression_data |>
    dplyr::mutate(label = is_swing) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$contextfeatures, label = "swing")
  
  xgb_hbp <- regression_data |>
    dplyr::filter(!is_swing) |>
    dplyr::mutate(label = is_hbp) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$contextfeatures, label = "hbp")
  }
  
  xgb_strike <- regression_data |>
    dplyr::filter(!is_swing, !is_hbp) |>
    dplyr::mutate(label = is_strike) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "strike")
  
  xgb_contact <- regression_data |>
    dplyr::filter(is_swing) |>
    dplyr::mutate(label = is_contact) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "contact")
  
  xgb_fair <- regression_data |>
    dplyr::filter(is_swing, is_contact) |>
    dplyr::mutate(label = is_fair) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "fair")
 
  xgb_hit <- regression_data |>
    dplyr::filter(is_swing, is_contact, is_fair, !is.na(hit_pred)) |>
    dplyr::mutate(label = hit_pred) |>
    train_pitch_outcome_xgb(features = config_pitch_outcome_xgb$features, label = "hit")


  # Combine models and return ----
  
  xgb <- list(
    swing = xgb_swing,
    hbp = xgb_hbp,
    strike = xgb_strike,
    contact = xgb_contact,
    fair = xgb_fair,
    hit = xgb_hit
  )

  model <- list(xgb = xgb, count_value = count_value, features = config_pitch_outcome_xgb$features)
  
  class(model) <- "pitch_outcome_model"

  return(model)  
}




#' Configuration for pitch outcome XGBoost models
#' 
#' Use this object to specify which features to include in the pitch outcome model and which values
#' to use for each tuning parameter. We have a separate script for determining optimal parameters.
#' 
config_pitch_outcome_xgb <- list(
  if(stuff==TRUE){
  features = c("pre_balls", "pre_strikes", "RHB", "strike_zone_top", "strike_zone_bottom",
               "plate_vx", "plate_vy", "plate_vz", "ax", "ay", "az", "extension"
  ),
  contextfeatures = c("pre_balls", "pre_strikes", "RHB", "strike_zone_top", "strike_zone_bottom"),
  }
  else{
      features = c("pre_balls", "pre_strikes", "RHB", "strike_zone_top", "strike_zone_bottom", "plate_x", "plate_z",
    "plate_vx", "plate_vy", "plate_vz", "ax", "ay", "az", "extension"
  ),
  }
  nrounds_swing = 150,
  params_swing = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic"),

  nrounds_hbp = 150,
  params_hbp = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic"),

  nrounds_strike = 150,
  params_strike = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic"),

  nrounds_contact = 150,
  params_contact = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic"),

  nrounds_fair = 150,
  params_fair = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "binary:logistic"),

  nrounds_hit = 150,
  params_hit = list(eta = 0.05, gamma = 0.1, max_depth = 9, objective = "reg:squarederror")
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
#' @param verbose argument passed directly to `xgboost::xgboost`, defaults to silent
#' @param ... additional parameters to pass to `xgboost::xgboost`
#' 
#' @return a fitted `xgboost::xgboost` model
#' 
train_pitch_outcome_xgb <- function(data_subset,
                                    features,
                                    label = c("swing", "hbp", "strike", "contact", "fair", "hit"),
                                    verbose = 0,
                                    ...) {

  label <- match.arg(label)

  xgb <- xgboost::xgboost(
    data = data_subset |>
      dplyr::select(dplyr::all_of(features)) |>
      as.matrix(),
    label = data_subset$label,
    nrounds = config_pitch_outcome_xgb[[glue::glue("nrounds_{label}")]],
    params = config_pitch_outcome_xgb[[glue::glue("params_{label}")]],
    verbose = verbose,
    ...
  )

  return(xgb)
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

  newdata <- newpitch |>
    dplyr::select(pre_balls, pre_strikes, RHB, strike_zone_top, strike_zone_bottom) |>    # make sure we don't duplicate columns
    dplyr::bind_cols(outcome_model_features) |>
    dplyr::select(dplyr::all_of(object$features)) |>
    as.matrix()

  pitch_pred <- tibble::tibble(
    pre_balls = newpitch$pre_balls,
    pre_strikes = newpitch$pre_strikes,
    RHB = newpitch$RHB,
    strike_zone_top = newpitch$strike_zone_top,
    strike_zone_bottom = newpitch$strike_zone_bottom,
    prob_swing = predict(object$xgb$swing, newdata = newdata[,colnames(newdata) %in% object$xgb$swing$feature_names]),
    prob_hbp = predict(object$xgb$hbp, newdata =  newdata[,colnames(newdata) %in% object$xgb$hbp$feature_names]),
    prob_strike = predict(object$xgb$strike,  newdata[,colnames(newdata) %in% object$xgb$strike$feature_names]),
    prob_contact = predict(object$xgb$contact, newdata[,colnames(newdata) %in% object$xgb$contact$feature_names]),
    prob_fair = predict(object$xgb$fair, newdata = newdata[,colnames(newdata) %in% object$xgb$fair$feature_names]),
    pred_hit = predict(object$xgb$hit, newdata = newdata[,colnames(newdata) %in% object$xgb$hit$feature_names])
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
