#' Estimates the model 
#' 
#' The function is a wrapper for several cmdlR functions that sets up parallel
#' processes and estimates the data. 
#'
#' @param inputs List of inputs to the estimation. The result of running
#' \code{prepare}. 
#' 
#' @export

estimate <- function(inputs) {
  time_start <- Sys.time()
  cat(black("Estimation started:", time_start, "\n"))
  model <- list()
  model[["time_start"]] <- time_start
  
  # Extract inputs ----
  estim_opt <- inputs[["estim_opt"]]
  model_opt <- inputs[["model_opt"]]
  save_opt <- inputs[["save_opt"]]
  summary_opt <- inputs[["summary_opt"]]
  db <- inputs[["db"]]
  workers <- inputs[["workers"]]
  
  # Attach the variables to enable calling by names ----
  attach()
  on.exit(detach(), add = TRUE)
  
  if (estim_opt$cores > 1) on.exit(parallel::stopCluster(workers), add = TRUE)
  
  # Estimate the model using the specified optimizer ----
  converged <- FALSE
  
  if (estim_opt$optimizer == "maxlik") {
    model_obj <- maxLik::maxLik()
    
    model[["ll"]] <- model_obj$i
    model[["coef"]] <- model_obj$coef
    model[["iterations"]] <- model_obj$nIter
    if (estim_opt$method == "bfgs" && model_obj$code %in% c(0)) converged <- TRUE 
    if (estim_opt$method == "bhhh" && model_obj$code %in% c(2, 8)) converged <- TRUE 
    if (estim_opt$method == "nr" && model_obj$code %in% c(0, 1, 2)) converged <- TRUE 
  }
  
  if (estim_opt$optimizer == "nloptr") {
    model_obj <- nloptr::nloptr()
    
    model[["ll"]] <- model_obj$objective
    model[["coef"]] <- model_obj$solution
    model[["iterations"]] <- model_obj$iterations
    if (model_obj$status %in% c(0)) converged <- TRUE
  }
  
  if (!converged) {
    stop("Model failed to converge. Estimation unsuccessful.\n")
  }
  
  # Calculate the hessian matrix ----
  model[["hessian"]] <- tryCatch({
    cat(reset$silver("Calculating the Hessian matrix. This may take a while. \n"))
    numDeriv::hessian()
  }, error = function(e) {
    NA
  })
  
  if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
    cat(red$bold("Failed: " %+% reset$silver("Hessian calculation using the \'numDeriv\' package.\n")))
    cat(reset$silver("Attempting to calculate Hessian using the \'maxLik\' package.\n"))
    model[["hessian"]] <- tryCatch({
      maxLik::maxLik(start = model[["coef"]], print.level = 0,
                     finalHessian = TRUE, method = estim_opt$method,
                     iterlime = 2, countIter = FALSE,
                     writeIter = FALSE, sumLL = FALSE)$hessian
    }, error = function(e) {
      NA
    })
  }
  
  # If the Hessian still cannot be calculated end and return the model object
  if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
    cat(red$bold("Failed: " %+% reset$silver("Hessian calculation failed or contains NA. Returning only some model information.\n")))
    time_end <- Sys.time()
    model[["time_end"]]
    return(model)
  }
  
  cat(green$bold("Success: " %+% reset$silver("Hessian calculated successfully.\n")))
  
  # Calculate the variance-covariance matrix ----
  model[["vcov"]] <- tryCatch({
    MASS::ginv(-model[["hessian"]])
  })
  
  if (estim_opt$robust_vcov && !is.null(model[["vcov"]])) {
    
  }
  
  # Capture time end and print completion message ----
  time_end <- Sys.time()
  cat(green$bold("Success: " %+% reset$silver("Estimation complete",
                                              time_end, "\n")))
  model[["time_end"]] <- time_end
  
  # Return the model object ----
  return(model)
}
