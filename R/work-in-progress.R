

#' Calculate the convergence criteria
#' 
#' @param object A model object of class 'cmdlr'
#'
#' @return A named numeric 
#' 
#' @export
convergence_criteria <- function(object, ...) {
  grad <- gradient(object)
  
  crit <- t(grad) %*% vcov(object) %*% grad
  
  attributes(crit) <- list(
    dim = NULL,
    names = "convergence_criteria"
  )
  
  return(
    crit
  )
}


