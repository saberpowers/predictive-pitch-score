#' Tune an XGBoost model while leveraging parallel computation
#'
#' This function is primarily a wrapper for `xgboost::xgb.cv`, which calculates CV errors for
#' provided parameter combinations. It leverages parallelization through
#' `future.apply::future_lapply`.
#' 
#' @param covariate_matrix a matrix of predictor variables to pass to `xgboost::xgb.DMatrix`
#' @param label a vector response variable to pass to `xgboost::xgb.DMatrix`
#' @param params_list a list of lists of parameters (each list of parameters is evaluated by CV)
#' @param nrounds integer number of boosting trees to pass to `xgboost::xgb.cv`
#' @param response character, must be "gaussian" or "binomial"
#' 
#' @return a dataframe of estimated CV errors indexed by parameters
#' 
tune_xgb_parallel <- function(covariate_matrix,
                              label,
                              params_list,
                              nrounds,
                              response = c("gaussian", "binomial")) {

  response <- match.arg(response)

  future::plan(strategy = future::multisession, workers = parallel::detectCores())
  cv_result_list <- future.apply::future_lapply(
    X = params_list,
    FUN = function(params, covariate_matrix, label, nrounds, response) {
      params$objective <- dplyr::case_when(
        response == "gaussian" ~ "reg:squarederror",
        response == "binomial" ~ "binary:logistic"
      )
      model_cv <- xgboost::xgb.cv(
        params = params,
        data = xgboost::xgb.DMatrix(data = covariate_matrix, label = label),
        nrounds = nrounds,
        nfold = 5,
        metrics = dplyr::case_when(
          response == "gaussian" ~ "rmse",
          response == "binomial" ~ "logloss"
        ),
        verbose = 0
      )
      result <- dplyr::bind_cols(tibble::as_tibble(params), model_cv$evaluation_log)
      return(result)
    },
    # Note future_lapply complains if we pass xgboost::xgb.DMatrix(covariate_matrix, label = label)
    # as an argument. It throws `Error: Cannot walk expression. Unknown object type ‘externalptr’`.
    covariate_matrix = covariate_matrix,
    label = label,
    nrounds = nrounds,
    response = response,
    future.seed = TRUE
  )
  future::plan(strategy = future::sequential)

  cv_result <- do.call(what = dplyr::bind_rows, args = cv_result_list)

  return(cv_result)
}
