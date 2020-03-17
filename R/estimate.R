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
  cat(paste0("Estimation started: ", time_start, "\n"))
  model <- list()
  model[["time_start"]] <- time_start
  
  # Extract inputs ----
  estim_opt <- inputs[["estim_opt"]]
  model_opt <- inputs[["model_opt"]]
  save_opt <- inputs[["save_opt"]]
  summary_opt <- inputs[["summary_opt"]]
  db <- inputs[["db"]]
  workers <- inputs[["workers"]]
  ll_func <- inputs[["log_lik"]]
  
  # Define the indices passed to the log-likelihood function ----
  indices <- list(
    N = length(unique(db[[model_opt[["id"]]]])),
    S = length(unique(db[[model_opt[["ct"]]]])),
    J = length(unique(db[[model_opt[["alt"]]]])),
    choice_var = db[[model_opt[["choice"]]]]
  )

  # Close the workers if the estimation fails
  if (estim_opt$cores > 1) on.exit(parallel::stopCluster(workers), add = TRUE)
  
  # Estimate the model using the specified optimizer ----
  converged <- FALSE
  param <- do.call(c, model_opt$param)
  
  if (tolower(estim_opt$optimizer) == "maxlik") {
    model_obj <- maxLik::maxLik(ll_func,
                                db = db,
                                indices = indices,
                                start = param,
                                method = estim_opt$method,
                                finalHessian = FALSE,
                                tol = estim_opt$tol, gradtol = estim_opt$gradtol,
                                reltol = estim_opt$reltol, 
                                steptol = estim_opt$steptol,
                                print.level = estim_opt$print_level, 
                                iterlim = estim_opt$iterlim)
    
    model[["ll"]] <- model_obj$maximum
    model[["coef"]] <- model_obj$estimate
    model[["iterations"]] <- model_obj$iterations
    model[["gradient"]] <- model_obj$gradient
    model[["gradient_obs"]] <- model_obj$gradientObs
    
    if (tolower(estim_opt$method) == "bfgs" && model_obj$code %in% c(0)) converged <- TRUE 
    if (tolower(estim_opt$method) == "bhhh" && model_obj$code %in% c(2, 8)) converged <- TRUE 
    if (tolower(estim_opt$method) == "nr" && model_obj$code %in% c(0, 1, 2)) converged <- TRUE 
  }
  
  if (tolower(estim_opt$optimizer) == "nloptr") {
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
  # Define the progress bar
  K <- length(model$coef)
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = 2 + 8 * (K * (K + 1) / 2),
    clear = FALSE, 
    width = 60
  )

  # Define the wrapper function
  ll_func_pb <- function(param, db, indices) {
    pb$tick()
    ll_func(param, db, indices)
  }
  
  # Try and catch if the Hessian cannot be calculated 
  model[["hessian"]] <- tryCatch({
    cat(blue$bold(symbol$info), "  Calculating the Hessian matrix. This may take a while. \n")
    numDeriv::hessian(func = ll_func_pb,
                      x = model$coef,
                      db = db,
                      indices = indices)
  }, error = function(e) {
    NA
  })
  
  # If the Hessian calculation failed, try calculating it using the maxLik package
  if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
    # Print messages to console
    cat(red$bold(symbol$cross), "  Hessian calculation using the \'numDeriv\' package.\n")
    cat(blue$bold(symbol$info), "  Trying to calculate Hessian using the \'maxLik\' package.\n")
    
    # Reset the progress bar
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :elapsed",
      total = 2 + 8 * (K * (K + 1) / 2),
      clear = FALSE, 
      width = 60
    )
    
    # Try calculating the hessian using the maxLik package
    model[["hessian"]] <- tryCatch({
      maxLik::maxLik(ll_func_pb,
                     start = model[["coef"]],
                     db = db,
                     indices = indices,
                     print.level = 0,
                     finalHessian = TRUE, method = estim_opt$method,
                     iterlim = 2)$hessian
    }, error = function(e) {
      NA
    })
  }
  
  # If the Hessian still cannot be calculated end and return the model object
  if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
    cat(red$bold(symbol$cross), "  Hessian calculation failed or contains NA. Returning only some model information.\n")
    time_end <- Sys.time()
    model[["time_end"]]
    return(model)
  }
  
  cat(green$bold(symbol$tick), "  Hessian calculated successfully.\n")
  
  # Calculate the variance-covariance matrix ----
  model[["vcov"]] <- tryCatch({
    MASS::ginv(-model[["hessian"]])
  })
  
  if (estim_opt$robust_vcov && !is.null(model[["vcov"]])) {
    
  }
  
  # Capture time end and print completion message ----
  time_end <- Sys.time()
  cat(green$bold(symbol$tick), paste0("  Estimation completed on ", time_end, "\n"))
  model[["time_end"]] <- time_end
  
  # Return the model object ----
  model
}
