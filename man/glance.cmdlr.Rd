% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/broom-generics.R
\name{glance.cmdlr}
\alias{glance.cmdlr}
\title{Glance at a cmdlr model object}
\usage{
\method{glance}{cmdlr}(x, ...)
}
\arguments{
\item{x}{A model object of class 'cmdlr'}

\item{...}{Additional parameters passed to glance}
}
\value{
A tidy [tibble::tibble()] summarizing the component-level information
about the model
}
\description{
A wrapper function around returning all fit
calculations as a tibble.
}
