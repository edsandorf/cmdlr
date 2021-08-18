#' S3 Generic for model summary
#'
#' @param object Model object
#' @param ... Other arguments passed to the function
#' 
#' @method summary cmdlr
#' 
#' @export
summary.cmdlr <- function(object, ...) {
  summarize(object)
}

#' S3 generic for coef
#'
#' @param object Model object
#' @param ... Other arguments passed to the function
#' 
#' @method coef cmdlr
#'
#' @export
coef.cmdlr <- function(object, ...) {
  result <- object$param_final
  return(result)
}

#' S3 generic for vcov
#'
#' @param object Model object
#' @param robust If TRUE return the robust vcov
#' @param ... Other arguments passed to the function
#' 
#' @method vcov cmdlr
#'
#' @export
vcov.cmdlr <- function(object, robust = FALSE, ...) {
  if (robust) {
    covmat <- object$robust_vcov
    
  } else {
    covmat <- object$vcov
  }
  
  return(covmat)
}

#' S3 Generic for printing choice shares
#' 
#' @param x An object of class choice_shares
#' @param ... Other parameters passed to print 
#' 
#' @export
# print.choice_shares <- function(x, ...) {
#   
# }
