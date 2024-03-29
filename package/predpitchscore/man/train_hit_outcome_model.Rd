% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_hit_outcome_model.R
\name{train_hit_outcome_model}
\alias{train_hit_outcome_model}
\title{Train hit outcome model}
\usage{
train_hit_outcome_model(pitch, play, base_out_run_exp, tune = FALSE)
}
\arguments{
\item{pitch}{dataframe of pitch data from \code{\link{extract_season}}}

\item{play}{dataframe of play data from \code{\link{extract_season}}}

\item{base_out_run_exp}{dataframe of base-out run expectancy from
\code{\link{compute_base_out_run_exp}}}

\item{tune}{logical, if true, tune the hyperparameters instead of fitting the model}
}
\value{
a fitted xgb.Booster object
}
\description{
Fit an XGBoost model to predict change in base-out run expectancy based on
launch speed (exit velo), launch angle and hit bearing.
}
