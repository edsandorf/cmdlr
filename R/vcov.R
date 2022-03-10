#' S3 generic for vcov
#' 
#' A generic method for obtaining the variance covariance matrix. It is a 
#' wrapper around the \link{\code{ginv}} from the MASS package to calculate the
#' (negative) inverse of the Hessian matrix. 
#'
#' @param object A model object of class 'cmdlr'
#' @param ... Other arguments passed to the function
#'
#' @return A matrix with row and column names equal to the parameters of the
#' fitted model 
#' 
#' @method vcov cmdlr
#'
#' @export
vcov.cmdlr <- function(object, ...) {
  vcov_matrix <- tryCatch({
    if (object[["control"]][["optimizer"]] %in% c("ucminf")) {
      x <- MASS::ginv(object[["hessian"]])
      
    } else {
      x <- MASS::ginv(-object[["hessian"]])
      
    }
    
    colnames(x) <- rownames(x) <- names(object[["param_free"]])
    
    return(x)
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to calculate variance-covariance matrix.")
    return(NA)
    
  })

  return(vcov_matrix)
}

#' A sandwich estimator for robust standard errors
#'
#' A straight forward sandwich estimator for robust standard errors with a 
#' correction based on the number of individuals/respondents in the data. The
#' variance-covariance matrix is based on the 'cmdlr' \link{\code{vcov}} 
#' function.
#'
#' @inheritParams vcov
#'
#' @return A matrix with row and column names equal to the parameters of the
#' fitted model 
#' 
#' @export
robust_vcov <- function(object, ...) {
  n_id <- nid(model)
  bread <- bread(model, n_id)
  meat <- meat(model, n_id)
  
  return(
    (bread %*% meat %*% bread) / n_id
  )
}

#' Create the bread of the sandwich
#' 
#' @inheritParams vcov
#' 
bread <- function(object, ...) {
  n_id <- nid(model)
  crumbs <- vcov(object) * n_id
  crumbs[is.na(crumbs)] <- 0
  
  return(crumbs)
}

#'Create the meat of the sandwich
#'
#' @inheritParams vcov
#'
meat <- function(object, ...) {
  n_id <- nid(model)
  adj <- n_id / (n_id - length(coef(object)))
  juices <- crossprod(gradient_obs(object)) / n_id
  juices[is.na(juices)] <- 0
  
  return(juices)
}
