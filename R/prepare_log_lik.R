#' Prepares the log likelihood function
#'
#' This function puts the finishing touches on the user supplied log likelihood
#' function and ensures that it can be run on a single core or multiple cores
#' using both the 'maxlik' and 'nloptr' packages.
#' 
#' @param lik Likelihood function
#' @param inputs List of inputs
#' @param workers A PSOCK cluster of workers 
#' 
#' @return A log likelihood wrapper functions

prepare_log_lik <- function(lik, inputs, workers) {
  # Get the fixed parameters
  param_fixed <- unlist(inputs$model_opt$param[inputs$model_opt$fixed])
  
  # Define the log_lik function (the inner part is the same for both parallel and serial)
  log_lik <- function(param) {
    lik(param, inputs)
  }
  environment(log_lik) <- new.env(parent = parent.env(environment(log_lik)))
  
  # Define the ll_func
  ll_func <- function(param_est) {
    # Combine estimated and fixed parameters into one vector - this implementation is strongly influenced by the 'apollo' package
    param <- c(param_est, param_fixed)
    
    # Check if we are running the model in parallel
    if (inputs$estim_opt$cores > 1) {
      ls_ll <- parallel::clusterCall(cl = workers,
                                     fun = log_lik,
                                     param = param)
      
      # Get the attributes and pass them along with ll
      attrbts <- lapply(ls_ll, function(x) attributes(x))
      ll <- Reduce(c, ls_ll)
      attributes(ll) <- list(attrbts = attrbts)
      ll
    } else {
      log_lik(param)
    }
  }
  
  # Return the log likelihood function
  cat(green$bold(symbol$tick), "  Log-likelihood function\n")
  ll_func
}
