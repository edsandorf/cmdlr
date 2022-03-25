#' Check if the Hessian matrix is complete
#' 
#' @param hess A hessian matrix
#'
#' @return A boolean equal to TRUE if the Hessian matrix could not be calculated
#' or contains missing values/NaN
hessian_complete <- function(hess) {
  return(
    ifelse(is.null(hess) || anyNA(hess), FALSE, TRUE)
  )
}

#' Checks if the supplied character string is used in the function
#' 
#' This assertion is useful for debugging and to check if necessary varibles
#' and parameters are included and used. 
#' 
#' @param x A character string or vector with one or more variables or 
#' parameters
#' @param ll A function
#' 
#' @examples 
#' ll <- function(param) {
#'   V <- list(
#'     alt1 = asc_1 + b_1 * var_1
#'   )
#' }
#' 
#' is_used("var_1", ll)
#' is_used("var_2", ll)
#' is_used("b_1", ll)
#' is_used(c("b_1", "var_1"), ll)
#' 
#' @export
is_used <- function(x, ll) {
  text <- deparse1(ll)
  regex <- paste0("\\b", x, "\\b")
  
  matches <- sapply(regex, grepl, text, perl = TRUE)
  names(matches) <- x
  
  return(
    matches
  )
}


