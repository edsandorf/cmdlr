% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate}
\alias{estimate}
\title{Function to estimate a choice model using cmdlr}
\usage{
estimate(ll, estim_env, model_options, control = NULL)
}
\arguments{
\item{ll}{This is the 'raw' log-likelihood function passed to the estimation
routine. It is important that the user takes into account whether the 
optimization routine is a minimizer (e.g. 'ucminf') or a maximizer 
(e.g. 'maxlik').}

\item{estim_env}{An estimation environment or list of estimation environments
returned from \code{\link{prepare}}}

\item{model_options}{A list of model options. Note, this list is validated
for a second time here to set some necessary defaults. See
\code{\link{validate}} for details.}

\item{control}{A list of control options that are passed to
\code{\link{set_controls}}.}
}
\value{
A 'cmdlr' model object
}
\description{
either maximize or minimize the log-likelihood function. If estimation 
is successful, the function calculates a high-precision numerical hessian
and a standard and robust variance-covariance matrix.
}
\details{
NOTE: There are no checks on whether the log-likelihood function is correctly
specified. If you see that your log-likelihood value is getting progressively
worse from starting values, check your 'll' specification and that it 
corresponds to the chosen optimizer.
}
\examples{
\dontrun{
  # See /examples for how to use
  estimate(ll, estim_env, model_options)
  
  # or 
  estimate(ll, estim_env, model_options, control)
}

}
