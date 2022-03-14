#' Search for starting values
#' 
#' The function searches for new starting values. Either in a random fashion or
#' based on a smarter search algorithm. The smart search algorithm is a rewritten
#' version of the search algorithm implemented in the Apollo package. The function
#' is rewritten to work with the current modeling framework.
#' 
#' @inheritParams estimate
#' @param type A character string indicating whether to use a simple or a smart
#' search algorithm. Default is 'simple'
#' @param n_candidates An integer giving the number of candidates to evaluate.
#' Default is 100.
#' @param n_return An integer giving the number of parameter vectors to return.
#' The default is 10
#' @param multiplier A double indicating a multiplier for the 'simple' search
#' algorithm. The default is 1. 
#' 
#' @return A matrix of starting values
#' 
#' @references 
#' Hess, S. & Palma, D., 2019, Apollo: A flexible, powerful and customisable f
#' reeware package for choice model estimation and application, Journal of 
#' Choice Modelling, 32
#' Bierlaire, M., Thémans, M. & Zufferey, N., 2010, A heuristic for nonlinear 
#' global optimization, INFORMS Journal on Computing, 22(1):
#' 
#' @export
search_start_values <- function(ll,
                                estim_env,
                                model_options,
                                control = NULL,
                                type = "simple",
                                n_candidates = 100,
                                n_return = 10, 
                                multiplier = 1) {
  
  cli::cli_h1("Searching for starting values")
  
  cores <- set_controls(control = control)[["cores"]]
  
  # Workers
  if (cores > 1) {
    # Create the cluster of workers and add stopCluster to on.exit()
    workers <- parallel::makeCluster(cores, type = "PSOCK")
    on.exit(parallel::stopCluster(workers), add = TRUE)
    
    # Prepare the workers
    pkgs <- c("maxLik", "numDeriv", "rlang") # Add cmdlR here
    parallel::clusterCall(workers, load_packages, pkgs)
    parallel::clusterExport(workers, "ll", envir = environment())
    parallel::parLapply(workers, estim_env, function(x) {
      assign("estim_env", x, envir = .GlobalEnv)
      return(NULL)
    })
    
  } else {
    workers <- NA
    
  }
  
  # Prepare the log-likelihood functions
  log_lik <- prepare_log_lik(ll, estim_env, workers)
  
  # Only search for parameters that are not fixed
  param <- unlist(model_options[["param"]])
  param_free <- param[!(names(param) %in% model_options[["fixed"]])]
  param_fixed <- param[model_options[["fixed"]]]
  
  start_values_free <- switch(type,
                              simple = search_simple(ll,
                                                     log_lik,
                                                     param_free,
                                                     param_fixed,
                                                     n_candidates,
                                                     n_return, 
                                                     multiplier,
                                                     workers),
                              smart = search_smart(ll,
                                                   log_lik,
                                                   param_free,
                                                   param_fixed,
                                                   n_candidates,
                                                   n_return, 
                                                   multiplier,
                                                   workers))
  
  start_values <- matrix(NA, nrow = n_return, ncol = length(param),
                         dimnames = list(NULL, names(param)))
  start_values[, names(param_free)] <- start_values_free
  start_values[, names(param_fixed)] <- rep_rows(t(param_fixed), n_return)
  
  # Print complete message and return the starting values as a named list
  cli::cli_alert_success("Search starting values complete")
  return(start_values)
}

#' Simple search
#'
#' The simple search algorithm is essentially an evaluation of a large number
#' of randomly generated vectors of starting values. Each vector is evaluated
#' at the starting values and sorted in descending order. The best fitting
#' vectors of starting values are returned.
#' 
#' @inheritParams search_start_values
#' @inheritParams estimate_maxlik
#' 
#' @return A matrix of starting values (free parameters only)
search_simple <- function(ll,
                          log_lik,
                          param_free,
                          param_fixed,
                          n_candidates,
                          n_return, 
                          multiplier,
                          workers) {
  
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = n_candidates,
    clear = FALSE, 
    width = 80
  )
  
  n_par <- length(param_free)
  
  start_values <- matrix(rep(param_free, times = n_candidates),
                         ncol = n_par,
                         byrow = TRUE) + 0.1
  
  rand_num <- matrix(runif(n_candidates * n_par), ncol = n_par) - 0.5
  
  start_values <- start_values + (start_values * rand_num * multiplier)
  
  # Turn into a list for faster processing
  list_start_values <- as.list(as.data.frame(t(start_values)))
  list_start_values <- lapply(list_start_values, function(x,
                                                          param_free) {
    names(x) <- names(param_free)
    return(x)
    
  }, param_free = param_free)
  
  start_values <- do.call(rbind, list_start_values)
  
  values <- lapply(list_start_values, function(param_free,
                                               param_fixed,
                                               workers,
                                               ll,
                                               return_sum,
                                               pb) {
    values <- log_lik(
      param_free,
      param_fixed,
      workers,
      ll,
      return_sum,
      pb
    )
    
    return(values)
  }, 
  param_fixed = param_fixed,
  workers = workers,
  ll = ll,
  return_sum = TRUE,
  pb = pb)
  
  values <- do.call(rbind, values)
  start_values <- cbind(start_values, values)
  
  start_values <- start_values[order(start_values[, ncol(start_values)], 
                                     decreasing = TRUE), ]
  
  return(start_values[seq_len(n_return), -ncol(start_values)])
}

#' Smart search
#' 
#' @inheritParams search_simple
#' 
#' @return A matrix of starting values (free parameters only)
search_smart <- function(ll,
                         log_lik,
                         param_free,
                         param_fixed,
                         n_candidates,
                         n_return, 
                         multiplier,
                         workers) {
  cat("Not implemented yet. Moving on ... \n")
}


