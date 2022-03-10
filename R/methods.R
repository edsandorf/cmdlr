#' S3 generic for coef
#'
#' @param object A model object of class 'cmdlr'
#' @param ... Other arguments passed to the function
#' 
#' @method coef cmdlr
#'
#' @export
coef.cmdlr <- function(object, ...) {
  result <- object[["param_final"]]
  return(result)
}

#' S3 generic for nobs
#' 
#' A generic method for getting the number of observations based on the number
#' of rows in the matrix of gradient observations.
#' 
#' @param object A model object of class 'cmdlr'
#' @param use.fallback Should fallback methods be used to try to guess the
#' value?
#' @param ... Additional arguments passed
#' 
#' 
#' @export
nobs <- function(object, use.fallback = FALSE, ...) {
  return(
    nrow(gradient_obs(object, ...))
  )
}

#' Function for extracting number of individuals
#' 
#' A generic method for getting the number of individuals based on the lenght of
#' the vector of optimum values
#'
#' @inheritParams nobs
#' 
#' @export
nid <- function(object, ...) {
  return(
    length(function_values(object, ...))
  )
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
