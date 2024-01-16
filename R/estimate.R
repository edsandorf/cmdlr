#' Function to estimate a choice model using cmdlr
#' 
#' either maximize or minimize the log-likelihood function. If estimation 
#' is successful, the function calculates a high-precision numerical hessian
#' and a standard and robust variance-covariance matrix. 
#' 
#' NOTE: There are no checks on whether the log-likelihood function is correctly
#' specified. If you see that your log-likelihood value is getting progressively
#' worse from starting values, check your 'll' specification and that it 
#' corresponds to the chosen optimizer. 
#' 
#' @param ll This is the 'raw' log-likelihood function passed to the estimation
#' routine. It is important that the user takes into account whether the 
#' optimization routine is a minimizer (e.g. 'ucminf') or a maximizer 
#' (e.g. 'maxlik').
#' @param estim_env An estimation environment or list of estimation environments
#' returned from \code{\link{prepare}}
#' @param model_options A list of model options. Note, this list is validated
#' for a second time here to set some necessary defaults. See
#' \code{\link{validate}} for details.
#' @param control A list of control options that are passed to
#' \code{\link{set_controls}}.
#' 
#' @return A 'cmdlr' model object 
#' 
#' @examples
#' \dontrun{
#'   # See /examples for how to use
#'   estimate(ll, estim_env, model_options)
#'   
#'   # or 
#'   estimate(ll, estim_env, model_options, control)
#' }
#' 
#' @export
estimate <- function(ll,
                     estim_env,
                     model_options,
                     control = NULL) {
  
  # SETUP ----
  estimation_start <- Sys.time()
  cli::cli_h1("Estimating the model")
  cli::cli_alert_info(paste0("Model estimation started: ", estimation_start))
  cli::cli_h2("Getting ready")
  
  # Set estimation controls
  control <- set_controls(control = control)
  cli::cli_alert_success("Estimation controls set")
  
  # WORKERS ----
  cores <- control[["cores"]]
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
    db_names_extra <- names(estim_env[[1L]])
    
  } else {
    workers <- NA
    db_names_extra <- names(estim_env)
    
  }
  
  # LOG LIKELIHOOD FUNCTION ----
  log_lik <- prepare_log_lik(ll, estim_env, workers)
  
  # Prepare the gradient function. Is this used anymore?
  # num_grad <- prepare_num_grad(ll, estim_env, workers)
  
  # ESTIMATE ----
  cli::cli_h2("Estimating")
  time_start_estimate <- Sys.time()
    
  param_start <- unlist(model_options[["param"]])
  param_free <- param_start[!(names(param_start) %in% model_options[["fixed"]])]
  # This is just for the model object. The actual parameters are loaded as
  # part of the estimation environment
  param_fixed <- param_start[model_options[["fixed"]]]
  
  
  model <- estimate_model(control[["optimizer"]],
                          log_lik,
                          param_start,
                          param_free,
                          param_fixed,
                          workers,
                          ll,
                          control)
  

  # Part of termination sequence in case of catastrophic model failure. 
  if (!get_convergence(model) || is.null(model)) {
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
  model[["optimum_values"]] <- log_lik(get_param_free(model))
  
  
  # OPTIMUM AT 0 ----
  ll_0 <- tryCatch({
    ll_0_tmp <- log_lik(get_param_free(model) * 0)
    
    switch(control[["optimizer"]],
           ucminf = -ll_0_tmp,
           maxlik = ll_0_tmp,
           bgw = log(ll_0_tmp)
    )
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to calculate 'll(0)'")
    return(NA)
  })
  
  model[["optimum_at_zero"]] <- sum(ll_0, na.rm = TRUE)
  
  # GRADIENT ----
  # We only derive the gradient if we are not using 'bgw'
  if (control[["optimizer"]] %in% c("maxlik", "ucminf")) {
    model[["gradient"]] <- numDeriv::grad(log_lik,
                                          get_param_free(model),
                                          return_sum = TRUE)
  }
  
  # JACOBIAN ----
  if (control[["calculate_jacobian"]]) {
    cli::cli_h2("Calculating the Jacobian (gradient observations)")
    time_start_jacobian <- Sys.time()
  
    if (control[["optimizer"]] == "bgw") {
      model[["gradient_obs"]] <- numDeriv::jacobian(log_lik,
                                                    get_param_free(model),
                                                    method = "simple",
                                                    return_log = TRUE) 
      
    } else {
      model[["gradient_obs"]] <- numDeriv::jacobian(log_lik,
                                                    get_param_free(model),
                                                    method = "simple")
      
    }
      
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
                                  get_param_free(model),
                                  control[["method"]],
                                  control[["optimizer"]])

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
                                    get_param_free(model),
                                    control[["method"]],
                                    control[["optimizer"]])
    }
    
    # If the Hessian still cannot be calculated end and return the model object
    if (!hessian_complete(model[["hessian"]])) {
      cli::cli_alert_danger(
        "Hessian calculation failed or contains NA."
      )
      
    } else {
      cli::cli_alert_success("Hessian calculated successfully")
      
    }
    
    # Print section time use
    time_diff <- Sys.time() - time_start_hessian
    cli::cli_alert_info(paste("Hessian calculation took",
                              round(time_diff, 2),
                              attr(time_diff, "units"),
                              sep = " "))
  
  } else {
    if (control[["optimizer"]] != "bgw") {
      model[["hessian"]] <- NA  
    }
  }
  
  # WRAP UP AND RETURN MODEL OBJECT ----
  cli::cli_h2("Wrapping up")
  
  model[["name"]] <- model_options[["name"]]
  model[["description"]] <- model_options[["description"]]
  # model[["model_frame"]] <- tibble::as_tibble(
  #   mget(estim_env[["db_names"]], envir = estim_env)
  # )
  model[["estimation_start"]] <- estimation_start
  model[["estimation_end"]] <- Sys.time()
  
  time_diff <- get_estimation_end(model) - get_estimation_start(model)
  cli::cli_alert_info(paste("Model estimation took",
                            round(time_diff, 2),
                            attr(time_diff, "units"),
                            sep = " "))
  
  cli::cli_alert_info(paste0("Model estimation completed on ",
                             model[["time_end"]]))
  
  return(model)
}
