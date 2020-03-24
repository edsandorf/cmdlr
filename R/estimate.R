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
  cat(blue$bold(symbol$info), bold(paste0("  Estimation started: ", time_start, "\n")))
  
  # Extract inputs ----
  estim_opt <- inputs[["estim_opt"]]
  model_opt <- inputs[["model_opt"]]
  save_opt <- inputs[["save_opt"]]
  summary_opt <- inputs[["summary_opt"]]
  db <- inputs[["db"]]
  indices <- inputs[["indices"]]
  workers <- inputs[["workers"]]
  ll_func <- inputs[["ll_func"]]
  num_grad <- inputs[["num_grad"]]
  
  # Close the workers if the estimation fails
  if (estim_opt$cores > 1) on.exit(parallel::stopCluster(workers), add = TRUE)
  
  # Create the model object ----
  model <- list()
  model[["name"]] <- model_opt$name
  model[["description"]] <- model_opt$description
  model[["method"]] <- tolower(estim_opt$method)
  model[["optimizer"]] <- tolower(estim_opt$optimizer)
  model[["cores"]] <- estim_opt$cores
  model[["R"]] <- model_opt$R
  model[["draws_type"]] <- model_opt$draws_type
  model[["time_start"]] <- time_start
  
  N <- model_opt$N
  S <- model_opt$S
  J <- model_opt$J
  
  model[["nobs"]] <- N * S
  
  # Estimate the model using the specified optimizer ----
  converged <- FALSE
  param <- do.call(c, model_opt$param)
  
  # If we have fixed parameters, we drop the fixed parameters from the vector of parameters - this implementation is heavily inspired by the 'apollo' package
  param_est <- param[!(names(param) %in% model_opt$fixed)]
  param_fixed <- param[model_opt$fixed]
    
  # maxLik package ----
  if (tolower(estim_opt$optimizer) == "maxlik") {
    model_obj <- maxLik::maxLik(ll_func,
                                start = param_est,
                                converged = FALSE,
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
    model[["message"]] <- model_obj$message

    if (tolower(estim_opt$method) == "bfgs" && model_obj$code %in% c(0)) converged <- TRUE 
    if (tolower(estim_opt$method) == "bhhh" && model_obj$code %in% c(2, 8)) converged <- TRUE 
    if (tolower(estim_opt$method) == "nr" && model_obj$code %in% c(0, 1, 2)) converged <- TRUE 
  }
  
  # ucminf package ----
  if (tolower(estim_opt$optimizer) == "ucminf") {
    model_obj <- ucminf::ucminf(par = param_est,
                                fn = ll_func,
                                hessian = 0,
                                converged = FALSE)
    
    # Added a minus to make the fit calculations correct
    model[["ll"]] <- -model_obj$value
    model[["coef"]] <- model_obj$par
    model[["message"]] <- model_obj$message
    model[["gradient"]] <- numDeriv::grad(ll_func, model$coef, converged = FALSE)
    
    if (model_obj$convergence %in% c(1, 2, 3, 4)) converged <- TRUE
  }
  
  # NLOPTR package ----
  if (tolower(estim_opt$optimizer) == "nloptr") {
    model_obj <- nloptr::nloptr(param, ll_func, num_grad,
                                opts = list(
                                  algorithm = estim_opt$method,
                                  print_level = estim_opt$print_level
                                ))
    
    # Added a minus to make the fit calculations correct
    model[["ll"]] <- -model_obj$objective
    model[["coef"]] <- model_obj$solution
    model[["iterations"]] <- model_obj$iterations
    
    if (model_obj$status %in% c(0)) converged <- TRUE
  }
  
  if (!converged) {
    stop("Model failed to converge. Estimation unsuccessful.\n")
  }
  
  # Add converged boolean to model object
  model[["converged"]] <- converged
  
  # Calculate the hessian matrix ----
  # Define the progress bar
  K <- length(model$coef)
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = 2 + 8 * (K * (K + 1) / 2),
    clear = FALSE, 
    width = 80
  )

  # Define the wrapper function
  ll_func_pb <- function(param) {
    pb$tick()
    ll_func(param, converged = TRUE)
  }
  
  # Try and catch if the Hessian cannot be calculated 
  model[["hessian"]] <- tryCatch({
    cat(blue$bold(symbol$info), "  Calculating the Hessian matrix. This may take a while. \n")
    hessian <- numDeriv::hessian(func = ll_func_pb, x = model$coef)
    colnames(hessian) <- names(model$coef)
    rownames(hessian) <- names(model$coef)
    hessian
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
      width = 80
    )
    
    # Try calculating the hessian using the maxLik package
    model[["hessian"]] <- tryCatch({
      hessian <- maxLik::maxLik(ll_func_pb,
                     start = model[["coef"]],
                     print.level = 0,
                     finalHessian = TRUE, method = estim_opt$method,
                     iterlim = 2)$hessian
      colnames(hessian) <- names(model$coef)
      rownames(hessian) <- names(model$coef)
      hessian
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
    if (inputs$estim_opt$optimizer == "ucminf") {
      vcov <- MASS::ginv(model[["hessian"]])
    } else {
      vcov <- MASS::ginv(-model[["hessian"]])
    }
    colnames(vcov) <- names(model$coef)
    rownames(vcov) <- names(model$coef)
    vcov
  })
  
  # Calculate the and model diagnostics ----
  model[["convergence_criteria"]] <- t(model$gradient) %*% model$vcov %*% model$gradient
  
  ll_0 <- tryCatch({
    ll_0_tmp <- ll_func((model$coef * 0), TRUE)
    if (tolower(estim_opt$optimizer) %in% c("nloptr", "ucminf")) {
      -ll_0_tmp
    } else {
      ll_0_tmp
    }
  }, error = function(e) {
    cat(red$bold(symbol$cross), "  Failed to calculate 'll_0'. Replacing with NA.\n")
    return(NA)
  })
  
  ll <- model[["ll"]]
  nobs <- model[["nobs"]]
  model[["ll_0"]] <- ll_0
  model[["adj_rho_sqrd"]] <- (1L - ((ll - K) / (ll_0)))
  model[["aic"]] <- ((-2L * ll) + (2L * K) )
  model[["aic3"]] <- ((-2L * ll) + (3L * K))
  model[["caic"]] <- ((-2L * ll) + (K * (log(nobs) + 1L)))
  model[["caic_star"]] <- ((-2L * ll) + (K * (log((nobs + 2L) / 24L) + 1L )))
  model[["ht_aic"]] <- ((-2L * ll) + (2L * K) + (((2L * (K + 1L)) * (K + 2L))/(nobs - K - 2L)))
  model[["bic"]] <- ((-2L * ll) + (K * log(nobs)))
  model[["bic_star"]] <- ((-2L * ll) + (K * (log((nobs + 2L) / 24L))))
  model[["dbic"]] <- ((-2L * ll) + (K * (log(nobs) - log(2L * pi))))
  model[["hqic"]] <- ((-2L * ll) + (2L * (K * (log(log(nobs))))))
  
  # Check if we are estimating a robust variance-covariance matrix
  if (estim_opt$robust_vcov && !is.null(model[["vcov"]])) {
    
  }
  
  # Capture time end and print completion message ----
  time_end <- Sys.time()
  cat(green$bold(symbol$tick), paste0("  Estimation completed on ", time_end, "\n"))
  model[["time_end"]] <- time_end
  
  # Return the model object ----
  model
}
