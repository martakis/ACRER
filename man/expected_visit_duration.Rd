% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expected_visit_duration.R
\name{expected_visit_duration}
\alias{expected_visit_duration}
\title{function to compute the expected visit duration per recreation area.}
\usage{
expected_visit_duration(dMinDurationAbs, dMaxDurationAbs, dMinDuration,
  dMaxDuration)
}
\arguments{
\item{dMinDurationAbs}{This is equal to_spend_minimum_time_per_activity (in min).}

\item{dMaxDurationAbs}{This is equal to_spend_maximum_time_per_activity (in min).}

\item{dMinDuration}{This is equal to max(maxduration,min_duration_abs)  (in min).}

\item{dMaxDuration}{This is equal to min(available_time_for_activity,step1)  (in min).}
}
\value{
expected_visit_duration Computation of the expected visit duration: list1a = matrix(dResult).
}
\description{
function to compute via integration the expected visit duration per recreation area.
}
\details{
expected_visit_duration(dMinDurationAbs, dMaxDurationAbs, dMinDuration,dMaxDuration).
}
