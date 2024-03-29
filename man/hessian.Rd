% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hessian.R
\name{hessian}
\alias{hessian}
\title{Calculate the Hessian}
\usage{
hessian(type, log_lik, param_free, method, optimizer)
}
\arguments{
\item{type}{A character string giving the method to use when approximating
the Hessian. One of: "numderiv" or "maxlik".}

\item{log_lik}{A log-likelihood expression prepared by 
\code{\link{prepare_log_lik}}}

\item{param_free}{A list of freely estimated parameters}

\item{method}{A method for the approximation of the Hessian. Only necessary
for the maxlik package}

\item{optimizer}{A character string giving the optimizer used}
}
\description{
The function is a wrapper around several methods for calculating the
numerical Hessian matrix. The function is meant for internal use and is not
exported.
}
