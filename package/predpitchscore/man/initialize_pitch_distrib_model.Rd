% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_pitch_distrib_model.R
\name{initialize_pitch_distrib_model}
\alias{initialize_pitch_distrib_model}
\title{Set initial parameter values for pitch distribution model}
\usage{
initialize_pitch_distrib_model(data_standardized, pitch_char_vec)
}
\arguments{
\item{data_standardized}{a dataframe of standardized pitch characteristics and context variables}

\item{pitch_char_vec}{character vector of pitch characteristics to model}
}
\value{
a list of initial values to pass directly to cmdstanr::cmdstan_model
}
\description{
Set initial parameter values for pitch distribution model
}
