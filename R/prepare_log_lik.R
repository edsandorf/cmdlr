#' Prepares the log likelihood function
#'
#' This function puts the finishing touches on the user supplied log likelihood
#' function and ensures that it can be run on a single core or multiple cores
#' using both the 'maxlik' and 'nloptr' packages.
#' 
#' @param log_lik Log likelihood function
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param workers A PSOCK cluster of workers 
#' 
#' @return A log likelihood wrapper functions

prepare_log_lik <- function(log_lik, estim_opt, model_opt, workers) {
  
  if (estim_opt$cores > 1) {
    # Define the parallell log-lik wrapper and assign to parent environment
    par_log_lik <- function(param) {
      log_lik(param, inputs)
    }
    environment(par_log_lik) <- new.env(parent = parent.env(environment(par_log_lik)))
    
    ll_func <- function(param) {
      ll <- do.call(sum,
                       parallel::clusterCall(cl = workers,
                                             fun = par_log_lik,
                                             param = param))
      
      if (tolower(estim_opt[["optimizer"]]) == "nloptr") {
        -ll
      } else {
        ll
      }
    }
  } else {
    ll_func <- function(param) {
      ll <- sum(log_lik(param, inputs))
      if (tolower(estim_opt[["optimizer"]]) == "nloptr") {
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
