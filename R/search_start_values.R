#' Function to search for starting values
#' 
#' @description
#' The function searches for new starting values. Either in a random fashion or
#' based on a smarter search algorithm. The smart search algorithm is a rewritten
#' version of the search algorithm implemented in the Apollo package. The function
#' is rewritten to work with the current modeling framework.
#' 
#' The function is intended for internal use only. 
#' 
#' @inheritParams estimate
#' @inheritParams prepare_log_lik
#' @param ll_func A complete and 'wrapped' log-likelihood function created by 
#' \code{\link{prepare_log_lik}}. 
#' 
#' @return A list of parameters
#' 
#' @references 
#' Hess, S. & Palma, D., 2019, Apollo: A flexible, powerful and customisable freeware package for choice model estimation and application, Journal of Choice Modelling, 32
#' Bierlaire, M., Thémans, M. & Zufferey, N., 2010, A heuristic for nonlinear global optimization, INFORMS Journal on Computing, 22(1):
#' 
search_start_values <- function(ll_func, estim_env, estim_opt, model_opt) {
  cat(blue$bold(symbol$info), bold("  Searching for starting values\n"))
  
  # Get the search options 
  search_options <- estim_opt$search_start_options
  
  # Only search for parameters that are not fixed
  param <- unlist(model_opt$param)
  param_est <- param[!(names(param) %in% model_opt$fixed)]
  param_fixed <- param[model_opt$fixed]
  
  # Set some useful parameters
  K <- length(param_est)
  N <- search_options$candidates
  
  # Prepare the log-likelihood function locally
  # ll_func <- inputs$ll_func
  
  if (search_options$simple_search) {
    # Define the progressbar
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :elapsed",
      total = N,
      clear = FALSE, 
      width = 80
    )
    
    # Create a matrix of potential parameters
    start_values <- matrix(rep(param_est, times = N), ncol = K, byrow = TRUE) + 0.1
    
    # Create a matrix of random numers of the same dims
    rand_num <- matrix(runif(N * K), ncol = K) - 0.5
    
    # Add the random numbers to the starting values 
    start_values <- start_values + (start_values * rand_num * search_options$multiplier)
    
    # Turn into a list for faster processing 
    start_values_list <- as.list(as.data.frame(t(start_values)))
    start_values_list <- lapply(start_values_list, function(x) {
      names(x) <- names(param_est)
      param_tmp <- param
      param_tmp[names(x)] <- x
      param_tmp
    })
    start_values <- Reduce(rbind, start_values_list)
    
    # Attache the data and draws 
    # attach(inputs$db)
    # on.exit(detach(inputs$db), add = TRUE)
    # if (inputs$model_opt$mixing) {
    #   attach(inputs$draws)
    #   on.exit(detach(inputs$draws), add = TRUE)
    # }
    
    # Evaluate ll_func at each value
    ll <- lapply(start_values_list, function(param) {
      pb$tick()
      sum(ll_func(param))
    })
    
    start_values <- cbind(start_values, Reduce(rbind, ll))
    start_values <- start_values[order(start_values[, ncol(start_values)], decreasing = TRUE), ]
    start_values <- start_values[1, , drop = TRUE][-ncol(start_values)]
    
  } else {
    cat("Not implemented yet. Moving on ... \n")
  }
  
  # Print complete message and return the starting values as a named list
  cat(green$bold(symbol$tick), "  Starting values\n")
  as.list(start_values)
}
