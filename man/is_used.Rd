% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{is_used}
\alias{is_used}
\title{Checks if the supplied character string is used in the function}
\usage{
is_used(x, func)
}
\arguments{
\item{x}{A character string or vector with one or more variables or 
parameters}

\item{func}{A function}
}
\description{
This assertion is useful for debugging and to check if necessary varibles
and parameters are included and used.
}
\examples{
ll <- function(param) {
  V <- list(
    alt1 = asc_1 + b_1 * var_1
  )
}

is_used("var_1", ll)
is_used("var_2", ll)
is_used("b_1", ll)
is_used(c("b_1", "var_1"), ll)

}
