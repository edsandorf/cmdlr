#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a cmdlr object
#'
#' @param x A model object of class 'cmdlr'
#' @param robust A boolean equal to TRUE if robust standard errors should be 
#' used and reported. The default value is FALSE
#' @param hypotheses A vector the length of the number of parameters (incl.
#' free and fixed) with values for the null hypothesis to test against. The
#' default is a two-sided test against 0. 
#' @param ... Additional parameters passed to tidy
#'
#' @method tidy cmdlr
#' 
#' @return A tidy [tibble::tibble()] summarizing the component-level information
#' about the model
#'
#' @export
tidy.cmdlr <- function(x, robust = TRUE, hypotheses = NULL, ...) {
  # Fix the vector of standard errors
  std_err <- structure(rep(NA, length(start_param(x))),
                       names = names(start_param(x)))
  
  std_err[names(free_param(x))] <- sqrt(diag(vcov(x, robust = robust)))
  
  # Check whether hypotheses are supplied
  if (is.null(hypotheses)) {
    hypotheses <- rep(0, length(std_err))
    
  } else {
    stopifnot(length(std_err) == length(hypotheses))
    
  }
  
  # Construct the tibble
  coef_tibble <- tibble::tibble(
    term = names(start_param(x)),
    estimate = final_param(x),
    std.err = std_err, 
    statistic = (estimate - hypotheses) / std_err,
    p.value = 2 * stats::pt(-abs(.data$statistic), df = nobs(x))
  )
  
  return(coef_tibble)
  
}

#' @importFrom generics glance
#' @export
generics::glance

#' Glance at a cmdlr model object
#'
#' A wrapper function around returning all fit
#' calculations as a tibble. 
#' 
#' @param x A model object of class 'cmdlr'
#' @param ... Additional parameters passed to glance
#'
#' @method glance cmdlr 
#' 
#' @return A tidy [tibble::tibble()] summarizing the component-level information
#' about the model
#'
#' 
#' @export
glance.cmdlr <- function(x, ...) {
  
  fit <- tibble::tibble(
    log_lik = x[["optimum"]], 
    convergence_crit = convergence_criteria(x, ...), 
    adj_rho_sqrd = model_fit(x, "adj_rho_sqrd"),
    aic = model_fit(x, "aic"),
    aic3 = model_fit(x, "aic3"),
    caic = model_fit(x, "caic"),
    caic_star = model_fit(x, "caic_star"),
    ht_aic = model_fit(x, "ht_aic"),
    bic = model_fit(x, "bic"),
    bic_star = model_fit(x, "bic_star"),
    dbic = model_fit(x, "dbic"),
    hqic = model_fit(x, "hqic")
  )
  
  return(fit)
}

#' @importFrom generics augment
#' @export
generics::augment

#' Augment a cmdlr object
#' 
#' Returns the predicted probabilities in a tidy format
#' 
#' @param x A model object of class 'cmdlr'
#' @param ... Additional parameters passed to glance
#' 
#' @method augment cmdlr
#' 
#' @export
augment.cmdlr <- function(x, ...) {
  stop("Not implemented yet")
}