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
  
  if (inputs$estim_opt$cores > 1) {
    # Define the parallell log-lik wrapper and assign to parent environment
    par_log_lik <- function(param) {
      log(lik(param, inputs))
    }
    environment(par_log_lik) <- new.env(parent = parent.env(environment(par_log_lik)))
    
    ll_func <- function(param) {
      ll <- do.call(sum,
                       parallel::clusterCall(cl = workers,
                                             fun = par_log_lik,
                                             param = param))
      
      if (tolower(inputs$estim_opt[["optimizer"]]) == "nloptr") {
        -ll
      } else {
        ll
      }
    }
  } else {
    ll_func <- function(param) {
      ll <- sum(log(lik(param, inputs)))
      if (tolower(inputs$estim_opt[["optimizer"]]) == "nloptr") {
        -ll
      } else {
        ll
      }
    }
  }
  
  # Return the log likelihood function
  cat(green$bold(symbol$tick), "  Log-likelihood function\n")
  ll_func
}
