#' Prepares the log likelihood function
#'
#' The function takes the user supplied log-likelihood function and creates
#' convenient wrappers that ensure the function can be evaluated both using a
#' single core and multiple cores. 
#' 
#' The function is meant for internal use only. 
#' 
#' @inheritParams estimate
#' @param estim_env A list of data and other variables to be coerced to environment with eval
#' @inheritParams prepare_workers
#' 
#' @return The estimation ready log likelihood function

prepare_log_lik <- function(ll, estim_env, model_opt, workers) {
  # Define wrapper for ll that includes the data mask
  log_lik <- function(param) {
    invisible(
      lapply(seq_along(param), function(i) {
        assign(names(param[i]), param[[i]], envir = estim_env)
      })
    )
    eval(body(ll), estim_env)
  }

  # Set the environment of the wrapper such that its parent environment is the
  # parent of this function (only necessary for parallel computing)
  if (!is.null(workers)) {
    environment(log_lik) <- environment(prepare_log_lik)
  }

  # Get the fixed parameters
  param_fixed <- unlist(model_opt$param[model_opt$fixed])

  # Define the ll_func
  ll_func <- function(param_free) {
    # Combine free and fixed parameters into one vector - see 'apollo' package
    # for details
    param <- c(param_free, param_fixed)

    # Check if we are running the model in parallel
    if (is.null(workers)) {
      log_lik(param)
    } else {
      ls_ll <- parallel::clusterCall(cl = workers,
                                     fun = log_lik,
                                     param = param)

      # Get the attributes and pass them along with ll
      attrbts <- lapply(ls_ll, function(x) attributes(x))
      ll <- Reduce(c, ls_ll)
      attributes(ll) <- list(attrbts = attrbts)
      ll
    }
  }
  
  # Return the log likelihood function
  message(green$bold(symbol$tick), "  Log-likelihood function")
  ll_func
}
