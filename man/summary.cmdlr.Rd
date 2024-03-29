% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.cmdlr}
\alias{summary.cmdlr}
\title{S3 Generic for model summary}
\usage{
\method{summary}{cmdlr}(object, robust = FALSE, ...)
}
\arguments{
\item{object}{A model object of class 'cmdlr'}

\item{robust}{A boolean equal to TRUE if you want to return a robust 
variance covariance matrix based on a simple sandwich estimator. The default
value is FALSE}

\item{...}{Other arguments passed to the function}
}
\description{
A generic summary function for a model object of class 'cmdlr'
}
