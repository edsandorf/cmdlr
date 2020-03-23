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
  
  # Define the log_lik function (the inner part is the same for both parallel and serial)
  log_lik <- function(param) {
    log(lik(param, inputs))
  }
  environment(log_lik) <- new.env(parent = parent.env(environment(log_lik)))
  
  # Define the ll_func
  ll_func <- function(param, converged) {
    if (inputs$estim_opt$cores > 1) {
      ll <- do.call(sum,
                    parallel::clusterCall(cl = workers,
                                          fun = log_lik,
                                          param = param))
    } else {
      ll <- sum(log_lik(param))
    }
    
    # Check which optimizer is used
    if (tolower(inputs$estim_opt[["optimizer"]]) %in% c("nloptr", "ucminf")) {
      # Print iterations if the model hasn't converged
      #if (!converged) {
      #  cat("Function value: ", -ll, "\n")  
      #}
      
      -ll
    } else {
      ll
    }
  }
  
  # Return the log likelihood function
  cat(green$bold(symbol$tick), "  Log-likelihood function\n")
  ll_func
}
