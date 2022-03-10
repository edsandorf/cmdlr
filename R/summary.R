#' S3 Generic for model summary
#'
#' @param object Model object
#' @param ... Other arguments passed to the function
#' 
#' @method summary cmdlr
#' 
#' @export
summary.cmdlr <- function(object, robust = TRUE, ...) {
  
  
  
  summary_object <- list(
    parameter_info = summary_parameter_info(object)
  )
  
  class(summary_object) <- "summary.cmdlr"
  return(summary_object)
}

#' Generic S3 print function
#' 
#' @inheritParams summary.cmdlr
#'
#' @method print summary.cmdlr
#'
print.summary.cmdlr <- function(x, ...) {
  cat("---- Parameter information ----\n")
  print(x[[1]])
  cat("\n\n")
}

#' Summary of parameter info
#' 
#' @inheritParams summary.cmdlr
#' 
#' @export
summary_parameter_info <- function(object, ...) {
  
  eigen_values <- grad <- rep(NA, length(object[["param_start"]]))
  names(eigen_values) <- names(grad) <- names(object[["param_start"]])
  
  # Calculate the Eigen values of the model object
  eigen_values[names(object[["param_free"]])] <- tryCatch({
    eigen(object[["hessian"]])[["values"]]
  },
  error = function(e) {
    return(
      rep(NA, nrow(object[["hessian"]]))
    )
  })
  
  # Fix gradient 
  grad[names(object[["param_free"]])] <- object[["gradient"]]
  
  param_info <- tibble::tibble(
    names = names(object[["param_start"]]),
    start = object[["param_start"]],
    final = object[["param_final"]],
    diff = final - start,
    grad = grad,
    eigen = eigen_values
  )
  
  return(param_info)
}

summary_coef_mat <- function(object, robust = TRUE, ...) {
  
  # Fix the vector of standard errors
  std_err <- structure(rep(NA, length(object[["param_start"]])),
                       names = names(object[["param_start"]]))
  
  if (robust) {
    std_err[names(object[["param_free"]])] <- sqrt(diag(robust_vcov(object)))
    
  } else {
    std_err[names(object[["param_free"]])] <- sqrt(diag(vcov(object)))
    
  }
  
  # Construct the tibble
  coef_tibble <- tibble::tibble(
    names = names(object[["param_start"]]),
    est = object[["param_final"]],
    std_err = std_err
  )
  
  return(coef_tibble)
  
}

# MUST BE CHANGED TO BE BROOM PACKAGE INTERFACE
summary_model_fit <- function(object, ...) {
  cat("Adj. Rho^2   ", model_fit(model, "adj_rho_sqrd"), "\n")
  cat("AIC          ", model_fit(model, "aic"), "\n")
  cat("AIC3         ", model_fit(model, "aic3"), "\n")
  cat("CAIC         ", model_fit(model, "caic"), "\n")
  cat("CAIC*        ", model_fit(model, "caic_star"), "\n")
  cat("HT-AIC/AICc  ", model_fit(model, "ht_aic"), "\n")
  cat("BIC          ", model_fit(model, "bic"), "\n")
  cat("BIC*         ", model_fit(model, "bic_star"), "\n")
  cat("DBIC         ", model_fit(model, "dbic"), "\n")
  cat("HQIC         ", model_fit(model, "hqic"), "\n")
}




#' Summarizes outputs
#'
#' This function is a wrapper for several small summary functions that provide
#' key summary statistics to help interpret the results of your model.
#'
#' @param model A model object returned by \code{estimate}
#'
#' @export

summarize <- function(model) {
  # Print model information ----
  cat("---- Information about the model ----\n")
  cat("Model name            ", model[["name"]], "\n")
  cat("Model description     ", model[["description"]], "\n")
  cat("Optimization package  ", model[["optimizer"]], "\n")
  cat("Optimization method   ", model[["method"]], "\n")
  cat("Number of cores used  ", model[["cores"]], "\n")
  cat("Number of draws used  ", model[["n_draws"]], "\n")
  cat("Type of draws used    ", model[["draws_type"]], "\n")
  cat("Convergence message   ", model[["message"]], "\n")
  cat("Convergence criteria  ", convergence_criteria(model), "\n")
  cat(paste0("Estimation started    ", model[["time_start"]], "\n"))
  cat(paste0("Estimation completed  ", model[["time_end"]], "\n"))
  cat("\n\n")

  # Define some helpful variables
  names_free <- names(model[["param_final"]])
  names_fixed <- names(model[["param_fixed"]])
  names_all <- names(model[["param_start"]])
  n_par <- length(model[["param_start"]])
  

  
  # Print the starting values ----


  # Print model summary statistics ----
  cat("---- Model diagnostics and fit ----\n")
  cat("LL           ", function_value(model), "\n")
  cat("LL(0)        ", function_value_zero(model), "\n")
  cat("n_obs        ", nobs(model), "\n")
  cat("n_par        ", n_par, "\n")

  cat("\n\n")

  cat("---- Parameter estimates ----\n")
  output <- matrix(NA, nrow = n_par, ncol = 11L)
  column_names <- c("est.", "s.e.", "t0", "p0", "t1", "p1",
                    "rob. s.e.", "rob. t0", "rob. p0", "rob. t1", "rob. p1")
  rownames(output) <- names_all
  colnames(output) <- column_names
  output[names_free, 1] <- model[["param_final"]]
  output[names_fixed, 1] <- model[["param_fixed"]]
  
  # Check if the variance-covariance matrix was computed
  if (!is.null(model[["vcov"]])) {
    output[names_free, 2] <- sqrt(diag(model[["vcov"]]))
    output[names_free, 3] <- output[names_free, 1]/output[names_free, 2]
    output[names_free, 4] <- 2 * stats::pt(-abs(output[names_free, 3]),
                                           df = model[["n_obs"]])
    output[names_free, 5] <- (1 - output[names_free, 1])/output[names_free, 2]
    output[names_free, 6] <- 2 * stats::pt(-abs(output[names_free, 5]),
                                           df = model[["n_obs"]])
  }

  # Check if the robust variance-covariance matrix was computed
  if (!is.null(model[["robust_vcov"]])) {
    output[names_free, 7] <- sqrt(diag(model[["robust_vcov"]]))
    output[names_free, 8] <- output[names_free, 1]/output[names_free, 7]
    output[names_free, 9] <- 2 * stats::pt(-abs(output[names_free, 8]),
                                           df = model[["n_obs"]])
    output[names_free, 10] <- (1 - output[names_free, 1])/output[names_free, 7]
    output[names_free, 11] <- 2 * stats::pt(-abs(output[names_free, 10]),
                                            df = model[["n_obs"]])
  }
  
  output[names_fixed, column_names[-1]] <- NA
  print(round(output, digits = 4))
  cat("\n\n")
}



## Additional crap for summary function
# model[["name"]] <- save_opt[["name"]]
# model[["description"]] <- save_opt[["description"]]
# model[["method"]] <- estim_opt[["method"]]
# model[["optimizer"]] <- estim_opt[["optimizer"]]
# model[["cores"]] <- estim_opt[["cores"]]
# model[["n_draws"]] <- model_opt[["n_draws"]]
# model[["draws_type"]] <- model_opt[["draws_type"]]
# model[["time_start"]] <- time_start
