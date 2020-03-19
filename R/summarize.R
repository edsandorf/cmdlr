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
  cat("---- Information about the model ----\n")
  cat("Model name:           ", model$name, "\n")
  cat("Model description:    ", model$description, "\n")
  cat("Optimization package: ", model$optimizer, "\n")
  cat("Optimization method:  ", model$method, "\n")
  cat("Number of cores used: ", model$cores, "\n")
  cat("Number of draws used: ", model$R, "\n")
  cat("Type of draws used:   ", model$draws_type, "\n")
  cat("Convergence message:  ", model$message, "\n")
  cat("Convergence criteria: ", model$convergence_criteria, "\n")
  cat(paste0("Estimation started:    ", model$time_start, "\n"))
  cat(paste0("Estimation completed:  ", model$time_end, "\n"))
  cat("\n\n")
  
  # Print the final gradient ----
  K <- length(model$gradient)
  D <- 5
  x <- c(model$gradient, rep(0, ((D * ceiling(K / D)) - K)))
  cat("---- Final gradient ----\n")
  print(matrix(x, ncol = D, byrow = TRUE))
  cat("\n\n")
  
  # Print model summary statistics ----
  cat("---- Model diagnostics and fit ----\n")
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
  
  cat("---- Parameter estimates ----\n")
  output <- matrix(0, nrow = K, ncol = 6L)
  output[, 1] <- model[["coef"]]
  output[, 2] <- sqrt(diag(model[["vcov"]]))
  output[, 3] <- output[, 1]/output[, 2]
  output[, 4] <- 2 * stats::pt(-abs(output[, 3]), df = model[["nobs"]])
  output[, 5] <- (1 - output[, 1])/output[, 2]
  output[, 6] <- 2 * stats::pt(-abs(output[, 5]), df = model[["nobs"]])
  rownames(output) <- names(model[["coef"]])
  colnames(output) <- c("Est.", "S.E.", "T0", "P0", "T1", "P1")
  print(output)
  cat("\n\n")
}

