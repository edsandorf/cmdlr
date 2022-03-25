#' Calculate the log likelihood ratio test for nested models
#' 
#' @param ... Models to test against
#' @param alpha The critical value of the test
#' 
#' @return A list with the test statistic, critical value and conclusion of 
#' the test 
#' 
#' @examples 
#' \dontrun{
#' ll_ratio_test(model1, model2, model3)
#' }
#' 
#' @export
ll_ratio_test <- function(..., alpha = 0.05) {
  
  models <- list(...)
  lls <- lapply(models, get_function_value)

  # Test statistic
  statistic <- structure(-2 * Reduce("-", lls),
                         names = "Chi Sqrd.")
  
  # Degrees of freedom 
  parameter <- structure(
    length(coef(models[[1]])) - length(coef(models[[2]])),
    names = "df"
  )
  
  # P value
  p.value <- stats::pchisq(statistic, parameter, lower.tail = FALSE)
  
  # Structure the LL values
  estimate <- structure(
    do.call(c, lls),
    names = c("LL unrestricted", paste0("LL nested ", seq_len(length(lls) - 1)))
  )
  
  result <- list(
    method = "Log likelihood ratio test for nested models", 
    null.value = "test",
    statistic = statistic,
    estimate = estimate,
    parameter = parameter,
    p.value = p.value,
    critical.value = stats::qchisq(alpha, 1, lower.tail = FALSE)
  )
  
  class(result) <- "htest"
  return(result)
}


#' A pairwise Chi-squared test
#' 
#' A pairwhise non-parametric chi-squared test for difference between grouped
#' categorical variables. 
#' 
#' @param x A response vector
#' @param g A grouping vector. If not a factor it will be converted to one.
#' @param p.adjust.method A character string specifying the method for multiple
#' testing adjustments. See \code{\link{p.adjust.methods}}
#' @param ... Additional arguments passed to \code{\link{chisq.test}}
#' 
#' @source Modified from here: https://stats.stackexchange.com/questions/85664/r-procedure-for-comparing-multiple-categorical-variables-similar-to-anova-fol
#' 
#' @examples
#' group <- rep(LETTERS[1:3], 10)
#' sex <- rep(c('Male', 'Female'), 15)
#' 
#' pairwise_chisq_test(sex, group, p.adjust.method = "none")
#'
#' @export
pairwise_chisq_test <- function(x,
                                g,
                                p.adjust.method = stats::p.adjust.methods,
                                ...) {
  # Get argument names
  data_name <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
  
  # Get p.adjust.method
  p.adjust.method <- match.arg(p.adjust.method)
  
  # Check if g is factor, if not coerce and print warning
  if (!is.factor(g)) {
    cli::cli_alert_warning("Coercing 'g' to factor. Check output.")
    g <- factor(g)
    
  }
  
  # Inner function to compare and extract the p-values from the chisq.test()
  compare.levels <- function(i, j) {
    xi <- x[as.integer(g) == i]
    xj <- x[as.integer(g) == j]
    tab <- cbind(table(xi), table(xj))
    
    return(
      stats::chisq.test(tab, ...)$p.value  
    )
  }
  
  # Get the p-values using a pairwise.table comparison
  p_value <- stats::pairwise.table(compare.levels, levels(g), p.adjust.method)
  
  # Prepare the list of results
  result <- list(method = "Chi-squared test",
                 data.name = data_name,
                 p.value = p_value, 
                 p.adjust.method = p.adjust.method)
  
  # Use existing S3 class for printing nicely formatted results
  class(result) <- "pairwise.htest"
  
  return(
    result
  )
  
}

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
