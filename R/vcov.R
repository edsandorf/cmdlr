#' @importFrom stats vcov
#' @export
stats::vcov

#' S3 generic for vcov
#' 
#' A generic method for obtaining standard and robust variance covariance matrix
#'
#' @param object A model object of class 'cmdlr'
#' @param robust A boolean equal to TRUE if you want to return a robust 
#' variance covariance matrix based on a simple sandwich estimator. The default
#' value is FALSE 
#' @param ... Other arguments passed to the function
#'
#' @return A matrix with row and column names equal to the parameters of the
#' fitted model 
#' 
#' @method vcov cmdlr
#'
#' @export
vcov.cmdlr <- function(object, robust = FALSE, ...) {
  if (robust) {
    return(robust_vcov(object))
    
  } else {
    return(normal_vcov(object))
    
  }
}

#' Normal variance-covariance matrix
#'
#' The function is a wrapper around the [MASS::] from the MASS package
#' to calculate the (negative) inverse of the Hessian matrix. 
#'
#' @inheritParams vcov.cmdlr
#'
#' @return A matrix with row and column names equal to the parameters of the
#' fitted model
normal_vcov <- function(object, ...) {
  return(
    tryCatch({
      if (object[["control"]][["optimizer"]] %in% c("ucminf")) {
        x <- MASS::ginv(object[["hessian"]])
        
      } else {
        x <- MASS::ginv(-object[["hessian"]])
        
      }
      
      colnames(x) <- rownames(x) <- names(free_param(object))
      
      return(x)
      
    }, error = function(e) {
      cli::cli_alert_danger("Failed to calculate variance-covariance matrix.")
      return(NA)
      
    })
  )
}

#' A sandwich estimator for robust standard errors
#'
#' A straight forward sandwich estimator for robust standard errors with a 
#' correction based on the number of individuals/respondents in the data.
#'
#' @inheritParams vcov.cmdlr
#'
#' @return A matrix with row and column names equal to the parameters of the
#' fitted model 
robust_vcov <- function(object, ...) {
  n_id <- nid(object)
  bread <- bread(object, n_id)
  meat <- meat(object, n_id)
  
  return(
    (bread %*% meat %*% bread) / n_id
  )
}

#' Create the bread of the sandwich
#' 
#' @inheritParams vcov.cmdlr
#' 
bread <- function(object, ...) {
  n_id <- nid(object)
  crumbs <- vcov(object) * n_id
  crumbs[is.na(crumbs)] <- 0
  
  return(crumbs)
}

#'Create the meat of the sandwich
#'
#' @inheritParams vcov.cmdlr
#'
meat <- function(object, ...) {
  n_id <- nid(object)
  adj <- n_id / (n_id - length(coef(object)))
  juices <- crossprod(gradient_obs(object)) / n_id
  juices[is.na(juices)] <- 0
  
  return(juices)
}
