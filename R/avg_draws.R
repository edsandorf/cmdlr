#' Average over draws
#'
#' Average over draws for the same respondent
#' 
#' @param pr_seq The probability of the sequence returned from
#' \code{panel_product}
#' @param N The number of individuals in the sample
#' 
#' @return A vector of likelihoods

avg_draws <- function(pr_seq, N) {
  # Reshape the matrix such that each row is an individual. Note the stacking
  pr_seq <- matrix(pr_seq, nrow = N)
  lik <- Rfast::rowmeans(pr_seq)
  return(lik)
}
