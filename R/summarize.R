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
  cat("Model name            ", model$name, "\n")
  cat("Model description     ", model$description, "\n")
  cat("Optimization package  ", model$optimizer, "\n")
  cat("Optimization method   ", model$method, "\n")
  cat("Number of cores used  ", model$cores, "\n")
  cat("Number of draws used  ", model$R, "\n")
  cat("Type of draws used    ", model$draws_type, "\n")
  cat("Convergence message   ", model$message, "\n")
  cat("Convergence criteria  ", model$convergence_criteria, "\n")
  cat(paste0("Estimation started    ", model$time_start, "\n"))
  cat(paste0("Estimation completed  ", model$time_end, "\n"))
  cat("\n\n")

  # Define some helpful variables
  names_free <- names(model[["param_final"]])
  names_fixed <- names(model[["param_fixed"]])
  names_all <- names(model[["param_start"]])
  K <- length(model[["param_start"]])
  
  # Print the starting values ----
  cat("---- Parameter information ----\n")
  output <- matrix(0, nrow = K, ncol = 4)
  rownames(output) <- names_all
  colnames(output) <- c("Start", "Final", "Diff", "Grad")
  output[, 1] <- model[["param_start"]]
  output[names_free, 2] <- model[["param_final"]]
  output[, 3] <- output[, 2] - output[, 1]
  output[names_free, 4] <- model[["gradient"]]
  print(round(output, digits = 4))
  cat("\n\n")

  # Print model summary statistics ----
  cat("---- Model diagnostics and fit ----\n")
  cat("LL           ", model[["ll"]], "\n")
  cat("LL(0)        ", model[["ll_0"]], "\n")
  cat("N            ", model[["nobs"]], "\n")
  cat("K            ", length(model[["param_final"]]), "\n")
  cat("Adj. Rho^2   ", model[["adj_rho_sqrd"]], "\n")
  cat("AIC          ", model[["aic"]], "\n")
  cat("AIC3         ", model[["aic3"]], "\n")
  cat("CAIC         ", model[["caic"]], "\n")
  cat("CAIC*        ", model[["caic_star"]], "\n")
  cat("HT-AIC/AICc  ", model[["ht_aic"]], "\n")
  cat("BIC          ", model[["bic"]], "\n")
  cat("BIC*         ", model[["bic_star"]], "\n")
  cat("DBIC         ", model[["dbic"]], "\n")
  cat("HQIC         ", model[["hqic"]], "\n")
  cat("\n\n")

  cat("---- Parameter estimates ----\n")
  output <- matrix(NA, nrow = K, ncol = 6L)
  rownames(output) <- names_all
  colnames(output) <- c("Est.", "S.E.", "T0", "P0", "T1", "P1")
  output[names_free, 1] <- model[["param_final"]]
  output[names_fixed, 1] <- model[["param_fixed"]]
  output[names_free, 2] <- sqrt(diag(model[["vcov"]]))
  output[names_free, 3] <- output[names_free, 1]/output[names_free, 2]
  output[names_free, 4] <- 2 * stats::pt(-abs(output[names_free, 3]), df = model[["nobs"]])
  output[names_free, 5] <- (1 - output[names_free, 1])/output[names_free, 2]
  output[names_free, 6] <- 2 * stats::pt(-abs(output[names_free, 5]), df = model[["nobs"]])
  output[names_fixed,  c("S.E.", "T0", "P0", "T1", "P1")] <- NA
  print(round(output, digits = 4))
  cat("\n\n")
}
