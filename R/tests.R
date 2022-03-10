#' Log likelihood ratio test
#' 
#' A log likelihood ratio test for nested models 
#' 
#' @param ... Nested models
#' @param alpha The critical value of the test
#' 
#' @return A list with the test statistic, critical value and conclusion of 
#' the test 
#' 
#' @export
ll_ratio_test <- function(..., alpha = 0.05) {
  lls <- lapply(list(...), function(x) x$ll)
  test_stat <- -2 * Reduce("-", lls)
  critical_value <- qchisq(alpha, 1, lower.tail = FALSE)
  conclusion <- ifelse(test_stat > critical_value, "reject", "fail to reject")
  
  return(
    list(
      test_stat = test_stat,
      critical_value = critical_value,
      conclusion = conclusion
    )
  )
  
}

#' A pairwise Chi-squared test
#' 
#' 
#' 
#' @param x A response vector
#' @param g A grouping vector. If not a factor it will be converted to one.
#' @param p.adjust.method A character string specifying the method for multiple
#' testing adjustments. See \link{\code{p.adjust.methods}}
#' @param ... Additional arguments passed to \link{\code{chisq.test}}
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
pairwise_chisq_test <- function(x, g, p.adjust.method = p.adjust.methods, ...) {
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
      chisq.test(tab, ...)$p.value  
    )
  }
  
  # Get the p-values using a pairwise.table comparison
  p_value <- pairwise.table(compare.levels, levels(g), p.adjust.method)
  
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
