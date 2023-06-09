% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outcome_tree.R
\name{get_outcome_tree}
\alias{get_outcome_tree}
\title{Get outcome tree}
\usage{
get_outcome_tree(description)
}
\arguments{
\item{description}{character vector, e.g. "In play, out(s)"}
}
\value{
dataframe with logical columns \code{is_swing}, \code{is_hbp}, \code{is_strike}, \code{is_contact}, \code{is_fair}
}
\description{
Get binary outcomes (swing, hbp, strike, contact, fair) from pitch outcome description.
}
