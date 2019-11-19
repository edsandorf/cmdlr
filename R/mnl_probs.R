#' Function for calculating the MNL probabilities
#'
#' Calculate the MNL probabilities taking the difference from the chosen
#' alternative.
#'
#' @param V A list of the observable part of utility
#' @param model_opt List of model options
#'
#' @export

mnl_probs <- function(V, model_opt) {
  
  # Extract the utility of the chosen alternative
  v_chosen <- Reduce("+", lapply(seq_along(V), function(j) {
    V[[j]] * (model_opt$choice == j)
  }))
  
  # Subtract the utility of the chosen alternative from all utilities
  V <- lapply(V, function(v) v - v_chosen)
  
  # Calculate the exponent and sum of utilities
  exp_v <- lapply(V, function(v) exp(v))
  sum_v <- Reduce("+", exp_v)
  
  # Calculate the probability of the chosen alt
  pr_chosen <- 1 / sum_v
  
  # Return the probability of the chosen alternative
  return(pr_chosen)
}
