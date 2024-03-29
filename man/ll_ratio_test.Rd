% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tests.R
\name{ll_ratio_test}
\alias{ll_ratio_test}
\title{Calculate the log likelihood ratio test for nested models}
\usage{
ll_ratio_test(..., alpha = 0.05)
}
\arguments{
\item{...}{Models to test against}

\item{alpha}{The critical value of the test}
}
\value{
A list with the test statistic, critical value and conclusion of 
the test
}
\description{
Calculate the log likelihood ratio test for nested models
}
\examples{
\dontrun{
ll_ratio_test(model1, model2, model3)
}

}
