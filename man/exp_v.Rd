% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability-helpers.R
\name{exp_v}
\alias{exp_v}
\title{Take the exponent of V}
\usage{
exp_v(V, alt_avail)
}
\arguments{
\item{V}{A list of utilities}

\item{alt_avail}{A list of alternative availabilities}
}
\description{
Takes the exponent of V while at the same time restricts the utility of 
unavailable alternatives to 0. This ensures that summing of utilities is only
over alternatives in the consideration set.
}
