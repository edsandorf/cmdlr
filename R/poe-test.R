#' Test for the difference in empirical distributions
#'
#' The function calculates the complete combinatorial of the supplied vectors
#' using a loop implementation by taking the difference between every 
#' combination of the elements in the vectors. The input vectors must 
#' be numeric, but can be of different lengths.
#' 
#' @param x An input vector
#' @param y An input vector
#' 
#' @return A numeric test value
#' 
#' @references 
#' Poe G. L., Giraud, K. L. & Loomis, J. B., 2005, Computational methods for 
#' measuring the difference of empirical distributions, American Journal of
#'  Agricultural Economics, 87:353-365
#' 
#' @examples 
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' poe_test(x, y)
#' 
#' @export
poe_test <- function (x, y) {
  if (!is.numeric(x)) stop("X must be numeric.")
  if (!is.numeric(y)) stop("Y must be numeric.")
  
  n_x <- length(x)
  n_y <- length(y)
  
  # Preallocate space to speed up looping
  v_diff <- rep(NA, n_x)
  for (n in seq_len(n_x)){
    v_diff[n] <- sum(x[n] - y < 0)
  }
  
  return(sum(v_diff) / (n_x * n_y))
}

#' Summary function for poe_test()
#' 
#' The function prints a more descriptive output to the console with the result
#' of the test for difference in empirical distributions.
#' 
#' @param x An estimate obtained from \code{\link{poe_test}}.
#'
#' @examples
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' test_stat <- poe_test(x, y)
#' summary_poe_test(test_stat)
#' 
#' @export
summary_poe_test <- function (x) {
  cat("Gamma: ", x, "\n")
  cat("Gamma >.95 and <.05 indicates difference at the 5% level.")
}
