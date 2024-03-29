% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_split_index}
\alias{get_split_index}
\title{Get the split index}
\usage{
get_split_index(db, cores)
}
\arguments{
\item{db}{A \code{data.frame()} or \code{tibble()} containing the data}

\item{cores}{An integer indicating the number of cores to use in estimation}
}
\value{
A list of row-indexes that can be used to split matrixes to different
cores
}
\description{
The function returns a split index based on the split data.frame passed to
the workers.
}
