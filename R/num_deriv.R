#' Obtain the numerical derivative by central difference
#' 
#' The function calculates the numerical derivate of an objective function using
#' central difference. It is a relatively quick way to obtain the score
#' contribution of each observation when the optimizer requires a single value.
#'
#' @param param A vector of parameters
#' @param log_lik The log-likelihood function
#' @param N The number of observations, e.g. individuals
#' @param eps The precision level
#'
#' @return A matrix with rows equal to N and columns equal to the number of
#' parameters
#'
#' @export

num_deriv <- function(param, log_lik, N, eps) {
  K <- length(param)
  score_contribution <- matrix(NA, nrow = N, ncol = K)
  
  # Loop over the parameters to calculate the numerical central difference
  for (i in seq_len(K)) {
    param_hi <- param
    param_lo <- param
    
    param_hi[i] <- param_hi[i] + eps
    param_lo[i] <- param_lo[i] - eps
    
    ll_value_hi <- log_lik(param_hi)
    ll_value_lo <- log_lik(param_lo)
    
    score_contribution[, i] <- (ll_value_hi - ll_value_lo) / (2 * eps)
  }
  return(score_contribution)
}
