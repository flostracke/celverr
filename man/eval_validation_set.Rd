% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluation.R
\name{eval_validation_set}
\alias{eval_validation_set}
\title{Title}
\usage{
eval_validation_set(forecasts_resamples, metricset, group_col = NULL)
}
\arguments{
\item{forecasts_resamples}{The forecasts against the resample set.}

\item{metricset}{A metricset created with \code{yardstick::metric_set()}}

\item{group_col}{A grouping column for calculating the metrics on a certain
level}
}
\value{
A tibble with the calculated metrics.
}
\description{
Title
}
