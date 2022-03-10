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
