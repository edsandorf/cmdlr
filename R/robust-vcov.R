#' A sandwich estimator for robust standard errors
#'
#' @param object A model object of class 'cmdlr'
#' @param ... Additional parameters passed to the function
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
#' @inheritParams robust_vcov
#' 
bread <- function(object, ...) {
  n_id <- nid(model)
  crumbs <- vcov(object) * n_id
  crumbs[is.na(crumbs)] <- 0
  
  return(crumbs)
}

#'Create the meat of the sandwich
#'
#' @inheritParams robust_vcov
#'
meat <- function(object, ...) {
  n_id <- nid(model)
  adj <- n_id / (n_id - length(coef(object)))
  juices <- crossprod(gradient_obs(object)) / n_id
  juices[is.na(juices)] <- 0
  
  return(juices)
}
