#' Ordered logit 
#'
#' This is a function to calculate an ordered logit model. 
#' 
#' The function is based on the core of the 'apollo_ol' from the Apollo package.
#'
#' @param ordered_outcome The ordered outcome variable
#' @param x A vector used to shift the the estimated thresholds. The default
#' value is 0, which implies no shifting. 
#' @param tau A vector of parameters. The length of the vector is equal to the 
#' number of levels of x - 1. No checks beyond 'non-conformable arrays' is 
#' written to ensure this is the case. 
#'@param lvls A vector giving the levels of the ordered outcome. If lvls is
#' missing (default), then the code assumes that they are ascending integers. 
#' 
#' 
#' 
#' 
#' @export
ordered_logit <- function(ordered_outcome, x, tau, lvls = NULL) {
  # Define the levels of the ordered variable and create a temporary ordered
  # outcome
  if (is.null(lvls)) lvls <- seq_len(length(tau) + 1)
  if (is.null(x)) x <- 0
  map <- stats::setNames(seq_along(lvls), lvls)
  index <- map[as.character(ordered_outcome)]
  
  # Set up the parameter vector to include -Inf and Inf as lower and upper bounds
  tau <- c(-Inf, tau, Inf)
  
  # Calculate the probability 
  prob <- 1 / (1 + exp(x - tau[index + 1])) - 1 / (1 + exp(x - tau[index]))
  
  # Return the probability
  prob
}
