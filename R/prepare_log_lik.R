#' Prepares the log likelihood function
#'
#' This function puts the finishing touches on the user supplied log likelihood
#' function and ensures that it can be run on a single core or multiple cores
#' using both the 'maxlik' and 'nloptr' packages.
#' 
#' @param log_lik Log likelihood function
#' @param estim_opt List of estimation options
#' @param workers A PSOCK cluster of workers 
#' 
#' @return A log likelihood wrapper functions

prepare_log_lik <- function(log_lik, estim_opt, workers) {
  
  if (estim_opt$cores > 1) {
    log_lik <- function(param) {
      value <- do.call(sum,
                       parallel::clusterCall(cl = workers, fun = log_lik, param = param))
      
      if (estim_opt[["optimizer"]] == "nloptr") {
        return(-value)
      } else {
        return(value)
      }
    }
  } else {
    log_lik <- function(param) {
      value <- sum(log_lik(param))
      if (estim_opt[["optimizer"]] == "nloptr") {
        return(-value)
      } else {
        return(value)
      }
    }
  }
  
  # Return the log likelihood function
  cat(green$bold(symbol$tick) %+% reset$silver("  Log-likelihood function\n"))
  return(log_lik)
}
