% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_pitch_outcome_model.R
\name{train_stuff_model}
\alias{train_stuff_model}
\title{Train Stuff model}
\usage{
train_stuff_model(pitch, pitch_value, tune = FALSE)
}
\arguments{
\item{pitch}{dataframe of pitch data from \code{\link{extract_season}}}

\item{pitch_value}{numeric vector of pitch values to regress onto Stuff characteristics}

\item{tune}{logical, if true, tune the hyperparameters instead of fitting the model}
}
\value{
a fitted xgb.Booster object
}
\description{
This function trains a gradient boosting model to predict pitch value (which is itself a
prediction from the pitch outcome model) from "Stuff" characteristics (no location or context).
}
