#' Function to estimate a choice model using cmdlR
#' 
#' The function is a wrapper around one of several optimization routines that
#' either maximize or minimize the log-likelihood function. If estimation 
#' is successful, the function calculates a high-precision numerical hessian
#' and a standard and robust variance-covariance matrix. 
#' 
#' NOTE: There are no checks on whether the log-likelihood function is correctly
#' specified. If you see that your log-likelihood value is getting progressively
#' worse from starting values, check your 'll' specification and that it 
#' corresponds to the chosen optmizer. 
#' 
#' @param ll This is the 'raw' log-likelihood function passed to the estimation
#' routine. It is important that the user takes into account whether the 
#' optimization routine is a minimizer (e.g. 'ucminf') or a maximizer 
#' (e.g. 'maxlik').
#' 
#' @param prepared_inputs A list of prepared inputs returned from 
#' \code{\link{prepare}}
#' 
#' @param validated_options A list of validated model options returned from 
#' \code{\link{validate}}
#' 
#' @return A list with model results including the hessian, standard and
#' robust variance-covariance matrixes and 
#' 
#' @examples
#' \dontrun{
#'   # See /examples for how to use
#'   estimate(ll, prepared_inputs, validated_options)
#' }
#' 
#' @export
estimate <- function(ll,
                     prepared_inputs,
                     validated_options) {
  # Initial preparations
  time_start <- Sys.time()
  cli::cli_h1("Estimating the model")
  cli::cli_alert_info(paste0("Model estimation started: ", time_start))
  cli::cli_h2("Getting ready")
  
  # Define useful variables and parameters
  estim_env <- prepared_inputs[["estim_env"]]
  control <- validated_options[["estim_opt"]]
  model_opt <- validated_options[["model_opt"]]
  save_opt <- validated_options[["save_opt"]]
  
  cores <- validated_options[["estim_opt"]][["cores"]]
  str_param_fixed <- validated_options[["model_opt"]][["fixed"]]
  
  # WORKERS ----
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
    
    # Get the nubmer of observations
    n_obs <- estim_env[[1L]][["n_obs"]]
    
  } else {
    workers <- NA
    n_obs <- estim_env[["n_obs"]]
  }
  
  # Prepare the log-likelihood functions----
  log_lik <- prepare_log_lik(ll, estim_env, workers)
  
  # Prepare the gradient function. Is this used anymore?
  # num_grad <- prepare_num_grad(ll, estim_env, workers)
  
  # ESTIMATE ----
  cli::cli_h2("Estimating")
  time_start_estimate <- Sys.time()

  # Define the sets of parameters
  param_start <- unlist(model_opt[["param"]])
  param_free <- param_start[!(names(param_start) %in% model_opt[["fixed"]])]
  param_fixed <- param_start[model_opt[["fixed"]]]
  
  model <- estimate_model(control[["optimizer"]],
                          log_lik,
                          param_start,
                          param_free,
                          param_fixed,
                          workers,
                          ll,
                          control)
  

  # Part of termination sequence in case of catastrophic model failure. 
  if (!model[["converged"]] || is.null(model)) {
    cli::cli_alert_danger("Model failed to converge. Returning model object.")
    return(model)
  }

  # Print section time use
  time_diff <- Sys.time() - time_start_estimate
  cli::cli_alert_info(paste("Model estimation took",
                            round(time_diff, 2),
                            attr(time_diff, "units"),
                            sep = " "))
  
  # OPTIMUM VALUES ----
  model[["optimum_values"]] <- log_lik(free_param(model),
                                       param_fixed,
                                       workers,
                                       ll,
                                       return_sum = FALSE,
                                       pb = NULL)
  # OPTIMUM AT 0 ----
  
  ll_0 <- tryCatch({
    ll_0_tmp <- log_lik(free_param(model) * 0,
                        param_fixed = param_fixed,
                        workers = workers,
                        ll =ll,
                        return_sum = TRUE,
                        pb = NULL)
    
    if (control[["optimizer"]] %in% c("ucminf")) {
      -ll_0_tmp
      
    } else {
      ll_0_tmp
    }
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to calculate 'll(0)'")
    return(NA)
  })
  
  model[["optimum_at_zero"]] <- ll_0
  
  # GRADIENT ----
  model[["gradient"]] <- numDeriv::grad(log_lik,
                                        free_param(model),
                                        param_fixed = param_fixed,
                                        workers = workers,
                                        ll = ll,
                                        return_sum = TRUE,
                                        pb = NULL)
  
  # JACOBIAN ----
  if (control[["calculate_jacobian"]]) {
    cli::cli_h2("Calculating the Jacobian (gradient observations)")
    time_start_jacobian <- Sys.time()
    
    model[["gradient_obs"]] <- numDeriv::jacobian(log_lik,
                                                  free_param(model),
                                                  param_fixed = param_fixed,
                                                  workers = workers,
                                                  ll = ll, 
                                                  return_sum = FALSE,
                                                  pb = NULL,
                                                  method = "simple")
    
    time_diff <- Sys.time() - time_start_jacobian
    cli::cli_alert_info(paste("Jacobian calculation took",
                              round(time_diff, 2),
                              attr(time_diff, "units"),
                              sep = " "))
    
  } else {
    model[["gradient_obs"]] <- NA
    
  }
  
  # HESSIAN ----
  if (control[["calculate_hessian"]]) {
    time_start_hessian <- Sys.time()
    cli::cli_h2("Calculating the Hessian matrix")

    # Calculate Hessian using the 'numderiv' package
    model[["hessian"]] <- hessian("numderiv",
                                  log_lik, 
                                  free_param(model),
                                  param_fixed,
                                  workers, 
                                  ll,
                                  control[["method"]])

    # If calculation failed, try different method for calculating the Hessian
    if (!hessian_complete(model[["hessian"]])) {
      cli::cli_alert_danger(
        "Hessian calculation using the \'numDeriv\' package"
      )
      
      cli::cli_alert_info(
        "Trying to calculate Hessian using the \'maxLik\' package"
      )
      
      model[["hessian"]] <- hessian("maxlik",
                                    log_lik, 
                                    free_param(model),
                                    param_fixed,
                                    workers, 
                                    ll,
                                    control[["method"]])
    }
    
    # If the Hessian still cannot be calculated end and return the model object
    if (!hessian_complete(model[["hessian"]])) {
      cli::cli_alert_danger(
        "Hessian calculation failed or contains NA. Returning model object"
      )
      
      time_end <- Sys.time()
      model[["time_end"]] <- time_end
      return(model)
    }
    
    cli::cli_alert_success("Hessian calculated successfully")
    
    # Print section time use
    time_diff <- Sys.time() - time_start_hessian
    cli::cli_alert_info(paste("Hessian calculation took",
                              round(time_diff, 2),
                              attr(time_diff, "units"),
                              sep = " "))
  
  } else {
    model[["hessian"]] <- NA

  }
  
  # WRAP UP AND RETURN MODEL OBJECT ----
  cli::cli_h2("Wrapping up")
  model[["time_start"]] <- time_start
  model[["time_end"]] <- Sys.time()
  time_diff <- model[["time_end"]] - time_start
  cli::cli_alert_info(paste("Model estimation took",
                            round(time_diff, 2),
                            attr(time_diff, "units"),
                            sep = " "))
  
  cli::cli_alert_info(paste0("Model estimation completed on ",
                             model[["time_end"]]))
  
  
  # Explicit return
  return(model)
}
