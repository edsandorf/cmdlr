#' Calculate the conditional probabilities
#'
#' Calculates the conditional class membership probabilities. Cannot calcualte
#' with continuous heterogeneity within class nor if it is part of an ICLV 
#' model.
#' 
#' @param pr_seq A matrix containing the class specific probability of the
#' choice sequence with dimensions n_id x n_cls where each column 
#' corresponds to a class.
#' 
#' @param pr_cls A matrix of unconditional class probabilities. Must have the
#' same dimensions as pr_seq
#'
#' @return The matrix of conditional class probabilities
lc_conditional <- function(pr_seq, pr_cls) {
  # Check that dimensions are correct
  stopifnot(dim(pr_seq) == dim(pr_cls))
  
  # Calculate the numerator
  numerator <- pr_seq * pr_cls
  pr_cond <- numerator / Rfast::rowsums(numerator)
  
  # Return
  return(pr_cond)
}

#' Calculate the unconditional probabilities
#' 
#' 
#' @return The matrix of unconditional class probabilities
lc_unconditional <- function() {
  
}
