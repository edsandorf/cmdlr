#' Calculate the MNL probability
#'
#' Function is convenient way to calculate the MNL probabilities.
#'
# mnl_probability <- function(utility,
#                             choice_var,
#                             alt_avail,
#                             type) {
#   
#   switch(type,
#          zero = mnl_zero(alt_avail),
#          calculate = mnl_calculate(),
#          conditional = mnl_conditional(),
#          gradient = mnl_gradient())
#   
# }

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


#' Calculate the probability at zero
#' 
#' Calculates the probability at zero parameter values only across the 
#' available alternatives. 
#' 
#' @param alt_avail List of alternative availabilities
#' 
#' @return A vector of length equal to the number of choice observations in 
#' the data (incl. NAs from the padding during data setup)
mnl_zero <- function(alt_avail) {
  n_alt_avail <- matrixStats::rowSums2(do.call(cbind, alt_avail), na.rm = TRUE)
  return(1 / n_alt_avail)
}
