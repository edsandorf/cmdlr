#' Calculate model fit
#' 
#' @param object A model object of class 'cmdlr'
#' @param type A character string indicating the fit statistic to calculate. Has
#' to be one of: "adj_rho_sqrd", "aic", "aic3", "caic", "caic_star", "ht_aic",
#' bic", "bic_star", "dbic" or "hqic". The default is "bic". 
#' @param ... Additional parameters passed to the function
#' 
#' @return A named numeric
#' 
#' @export
model_fit <- function(object, type = "bic", ...) {
  # Current supported types
  types <- c("adj_rho_sqrd", "aic", "aic3", "caic", "caic_star", "ht_aic",
             "bic", "bic_star", "dbic", "hqic")
  
  # Check if valid type
  if (!(type %in% types)) {
    cli::cli_alert_warning(
      paste0("Unknown model fit type: '", type, "'")
    )
    
    return(NA)
    
  }
  
  fit <- switch(type,
    adj_rho_sqrd = adj_rho_sqrd(object, ...),
    aic = aic(object, ...),
    aic3 = aic3(object, ...),
    caic = caic(object, ...),
    caic_star = caic_star(object, ...),
    ht_aic = ht_aic(object, ...),
    bic = bic(object, ...),
    bic_star = bic_star(object, ...),
    dbic = dbic(object, ...),
    hqic = hqic(object, ...)
  )
  
  names(fit) <- type
  # class(fit) <- c("model_fit", "numeric")
  return(fit)
}

#' Calculate Adjusted Rho Squared
#' 
#' @inheritParams model_fit
adj_rho_sqrd <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  maximum_zero <- get_function_value_zero(object)
  
  return(
    1 - ((maximum - n_par) / maximum_zero)
  )
}

#' Calculate AIC
#' 
#' @inheritParams model_fit
aic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  
  return(
    -2 * maximum + (2 * n_par)
  )
}

#' Calculate AIC3
#' 
#' @inheritParams model_fit
aic3 <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  
  return(
    -2 * maximum + (3 * n_par)
  )
}

#' Calculate CAIC
#' 
#' @inheritParams model_fit
caic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum + (n_par * (log(n_obs) + 1))
  )
}

#' Calculate CAIC*
#' 
#' @inheritParams model_fit
caic_star <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum + (n_par * (log((n_obs + 2) / 24) + 1))
  )
}

#' Calculate HTAIC
#' 
#' @inheritParams model_fit
ht_aic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum  + (2 * n_par) + (((2 * (n_par + 1)) * (n_par + 2)) / (n_obs - n_par - 2))
  )
}

#' Calculate BIC
#' 
#' @inheritParams model_fit
bic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum + (n_par * log(n_obs))
  )
}

#' Calculate BIC*
#' 
#' @inheritParams model_fit
bic_star <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum+ (n_par * (log((n_obs + 2) / 24)))
  )
}

#' Calculate DBIC
#' 
#' @inheritParams model_fit
dbic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum + (n_par * (log(n_obs) - log(2 * pi))) 
  )
}

#' Calculate HQIC
#' 
#' @inheritParams model_fit
hqic <- function(object, ...) {
  n_par <- length(coef(object))
  maximum <- get_function_value(object)
  n_obs <- nobs(object)
  
  return(
    -2 * maximum + (2 * (n_par * (log(log(n_obs)))))
  )
}
