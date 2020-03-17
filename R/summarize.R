#' Summarizes outputs
#'
#' This function is a wrapper for several small summary functions that provide
#' key summary statistics to help interpret the results of your model. 
#'
#' @param model A model opbject returned by \code{estimate}
#' @param summary_opt A list of summary options
#' 
#' @export

summarize <- function(model, summary_opt) {
  # Print model information ----
  cat("Model name:           ", model$name, "\n")
  cat("Model description:    ", model$description, "\n")
  cat("Optimization package: ", model$optimizer, "\n")
  cat("Optimization method:  ", model$method, "\n")
  cat("Number of cores used: ", model$cores, "\n")
  cat("Convergence message:  ", model$message, "\n")
  cat("Convergence criteria: ", model$convergence_criteria, "\n")
  cat("\n\n")
  
  # Print the final gradient ----
  k <- length(model$gradient)
  d <- 5
  x <- c(model$gradient, rep(0, ((d * ceiling(k / d)) - k)))
  cat("Final gradient \n")
  print(matrix(x, ncol = d, byrow = TRUE))
  cat("\n\n")
  
  # Print model summary statistics ----
  cat("Model diagnostics and fit\n")
  cat("LL:          ", model[["ll"]], "\n")
  cat("LL(0):       ", model[["ll_0"]], "\n")
  cat("N:           ", model[["nobs"]], "\n")
  cat("K:           ", length(model[["coef"]]), "\n")
  cat("Adj. Rho^2:  ", model[["adj_rho_sqrd"]], "\n")
  cat("AIC:         ", model[["aic"]], "\n")
  cat("AIC3:        ", model[["aic3"]], "\n")
  cat("CAIC:        ", model[["caic"]], "\n")
  cat("CAIC*:       ", model[["caic_star"]], "\n")
  cat("HT-AIC/AICc: ", model[["ht_aic"]], "\n")
  cat("BIC:         ", model[["bic"]], "\n")
  cat("BIC*:        ", model[["bic_star"]], "\n")
  cat("DBIC:        ", model[["dbic"]], "\n")
  cat("HQIC:        ", model[["hqic"]], "\n")
  cat("\n\n")
  
  
}

