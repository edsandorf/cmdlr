#' Summarizes outputs
#'
#' This function is a wrapper for several small summary functions that provide
#' key summary statistics to help interpret the results of your model.
#'
#' @param model A model opbject returned by \code{estimate}
#' @param inputs A list of summary options
#'
#' @export

summarize <- function(model, inputs) {
  summary_opt <- inputs$summary_opt

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

  # Print the starting values ----
  cat("---- Parameter information ----\n")
  output <- matrix(0, nrow = length(inputs$model_opt$param), ncol = 4)
  rownames(output) <- names(inputs$model_opt$param)
  colnames(output) <- c("Start", "Final", "Diff", "Grad")
  coef_est_names <- names(model$coef)
  output[coef_est_names, 1] <- model$coef_start
  output[coef_est_names, 2] <- model$coef
  output[, 3] <- output[, 2] - output[, 1]
  output[coef_est_names, 4] <- model$gradient
  print(round(output, digits = 4))
  cat("\n\n")

  # Print model summary statistics ----
  cat("---- Model diagnostics and fit ----\n")
  cat("LL           ", model[["ll"]], "\n")
  cat("LL(0)        ", model[["ll_0"]], "\n")
  cat("N            ", model[["nobs"]], "\n")
  cat("K            ", length(model[["coef"]]), "\n")
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
  output <- matrix(NA, nrow = length(inputs$model_opt$param), ncol = 6L)
  rownames(output) <- names(inputs$model_opt$param)
  colnames(output) <- c("Est.", "S.E.", "T0", "P0", "T1", "P1")
  names_est <- names(model[["coef"]])
  names_fixed <- rownames(output) %in% inputs$model_opt$fixed
  output[names_est, 1] <- model[["coef"]]
  output[names_fixed] <- unlist(inputs$model_opt$param)[names_fixed]
  output[names_est, 2] <- sqrt(diag(model[["vcov"]]))
  output[names_est, 3] <- output[names_est, 1]/output[names_est, 2]
  output[names_est, 4] <- 2 * stats::pt(-abs(output[names_est, 3]), df = model[["nobs"]])
  output[names_est, 5] <- (1 - output[names_est, 1])/output[names_est, 2]
  output[names_est, 6] <- 2 * stats::pt(-abs(output[names_est, 5]), df = model[["nobs"]])
  output[names_fixed,  c("S.E.", "T0", "P0", "T1", "P1")] <- NA
  print(round(output, digits = 4))
  cat("\n\n")
}
