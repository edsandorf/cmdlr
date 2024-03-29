% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tests.R
\name{pairwise_chisq_test}
\alias{pairwise_chisq_test}
\title{A pairwise Chi-squared test}
\source{
Modified from here: https://stats.stackexchange.com/questions/85664/r-procedure-for-comparing-multiple-categorical-variables-similar-to-anova-fol
}
\usage{
pairwise_chisq_test(x, g, p.adjust.method = stats::p.adjust.methods, ...)
}
\arguments{
\item{x}{A response vector}

\item{g}{A grouping vector. If not a factor it will be converted to one.}

\item{p.adjust.method}{A character string specifying the method for multiple
testing adjustments. See \code{\link{p.adjust.methods}}}

\item{...}{Additional arguments passed to \code{\link{chisq.test}}}
}
\description{
A pairwhise non-parametric chi-squared test for difference between grouped
categorical variables.
}
\examples{
group <- rep(LETTERS[1:3], 10)
sex <- rep(c('Male', 'Female'), 15)

pairwise_chisq_test(sex, group, p.adjust.method = "none")

}
