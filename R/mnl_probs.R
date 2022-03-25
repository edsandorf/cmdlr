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
#' @param V A list of utilities
#' @param choice_var A vector with the chosen alternative
#' @param alt_avail A list of alternative availabilities
#'
#' @export
mnl_probabilities <- function(V, choice_var, alt_avail) {
  # Get the utility of the chosen alternative and 
  v_chosen <- get_v_chosen(V, choice_var)
  
  # Subtract it from all utilities
  V <- difference_utility(V, v_chosen)
  
  # Exponentiate all the utilities
  V <- exp_v(V, alt_avail)
  
  sum_v <- Reduce("+", V)
  
  # Return the probability of the chosen alternative
  return(lapply(V, function(v, sum_v) v / sum_v, sum_v = sum_v))
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
