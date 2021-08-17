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
  estim_opt <- validated_options[["estim_opt"]]
  model_opt <- validated_options[["model_opt"]]
  save_opt <- validated_options[["save_opt"]]
  
  cores <- validated_options[["estim_opt"]][["cores"]]
  str_param_fixed <- validated_options[["model_opt"]][["fixed"]]
  
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
    
    # Get the nubmer of observations
    n_obs <- estim_env[[1L]][["n_obs"]]
    
  } else {
    workers <- NA
    n_obs <- estim_env[["n_obs"]]
  }
  
  # Prepare the log-likelihood functions
  log_lik <- prepare_log_lik(ll, estim_env, workers)
  
  # Prepare the gradient function
  num_grad <- prepare_num_grad(ll, estim_env, workers)
  
  # Estimate the model
  cli::cli_h2("Estimating")
  time_start_estimate <- Sys.time()
  
  # Prepare the starting parameters by separating the free and fixed parameters
  # into two vectors - see the 'apollo' package for details. 
  param <- unlist(model_opt[["param"]])
  param_free <- param[!(names(param) %in% model_opt[["fixed"]])]
  param_fixed <- param[model_opt[["fixed"]]]
  n_par <- length(param_free)
  
  # Estimate the model using the 'maxLik' package
  model <- switch(
    estim_opt[["optimizer"]],
    maxlik = estimate_maxlik(log_lik,
                             param_free,
                             param_fixed,
                             workers,
                             ll,
                             return_sum = FALSE,
                             pb = NULL,
                             estim_opt),
    ucminf = estimate_ucminf(log_lik,
                             param_free,
                             param_fixed,
                             workers,
                             ll,
                             return_sum = TRUE,
                             pb = NULL,
                             estim_opt)
  )
  
  # Add other information to the model object
  class(model) <- "cmdlr"
  model[["name"]] <- save_opt[["name"]]
  model[["description"]] <- save_opt[["description"]]
  model[["method"]] <- estim_opt[["method"]]
  model[["optimizer"]] <- estim_opt[["optimizer"]]
  model[["cores"]] <- estim_opt[["cores"]]
  model[["n_draws"]] <- model_opt[["n_draws"]]
  model[["draws_type"]] <- model_opt[["draws_type"]]
  model[["time_start"]] <- time_start
  model[["nobs"]] <- model_opt$nobs
  model[["param_start"]] <- param
  model[["param_fixed"]] <- param_fixed
  
  # If set to converge at starting values
  if (estim_opt$iterlim %in% c(0, 1)) {
    model[["converged"]] <- TRUE
  }
  
  # Check for non-convergence
  if (!model[["converged"]]) {
    cli::cli_alert_danger("Model failed to converge. Returning model object.")
    return(model)
  }

  # Add the log-likelihood values and attributes to the model object ----
  model[["ll_values"]] <- log_lik(model[["param_final"]],
                                  param_fixed,
                                  workers,
                                  ll,
                                  return_sum = FALSE,
                                  pb = NULL)
  
  n_id <- length(model[["ll_values"]])
  
  # Print section time use
  time_diff <- Sys.time() - time_start_estimate
  cli::cli_alert_info(paste("Model estimation took",
                            round(time_diff, 2),
                            attr(time_diff, "units"),
                            sep = " "))
  
  # Hessian
  if (estim_opt[["calculate_hessian"]]) {
    time_start_hessian <- Sys.time()
    cli::cli_h2("Calculating the Hessian matrix")
    
    # Define the progress bar
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :elapsed",
      total = 2 + 8 * (n_par * (n_par + 1) / 2),
      clear = FALSE, 
      width = 80
    )
    
    # Try and catch if the Hessian cannot be calculated 
    model[["hessian"]] <- tryCatch({
      hessian <- numDeriv::hessian(log_lik,
                                   model[["param_final"]],
                                   param_fixed = param_fixed,
                                   workers = workers,
                                   ll = ll, 
                                   return_sum = TRUE,
                                   pb = pb)
      colnames(hessian) <- names(model[["param_final"]])
      rownames(hessian) <- names(model[["param_final"]])
      
      hessian
      
    }, error = function(e) {
      return(NA)
    })
    
    # If the Hessian calculation failed, try calculating it using the maxLik
    # package
    if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
      # Print messages to console
      cli::cli_alert_danger(
        "Hessian calculation using the \'numDeriv\' package"
      )
      cli::cli_alert_info(
        "Trying to calculate Hessian using the \'maxLik\' package"
      )

      # Reset the progress bar
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent :elapsed",
        total = 2 + 8 * (n_par * (n_par + 1) / 2),
        clear = FALSE, 
        width = 80
      )
      
      # Try calculating the hessian using the maxLik package
      model[["hessian"]] <- tryCatch({
        hessian <- maxLik::maxLik(log_lik,
                                  start = model[["param_final"]],
                                  param_fixed = param_fixed,
                                  workers = workers,
                                  ll = ll,
                                  return_sum = TRUE,
                                  pb = pb,
                                  print.level = 0,
                                  finalHessian = TRUE,
                                  method = estim_opt[["method"]],
                                  iterlim = 2)[["hessian"]]
        
        colnames(hessian) <- names(model[["param_final"]])
        rownames(hessian) <- names(model[["param_final"]])
        hessian
        
      }, error = function(e) {
        return(NA)
      })
    }
    
    # If the Hessian still cannot be calculated end and return the model object
    if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
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
    
    # Variance covariance matrix
    cli::cli_h2("Calculating the VCOV matrix")
    time_start_vcov <- Sys.time()
    
    # Standard
    model[["vcov"]] <- tryCatch({
      if (estim_opt[["optimizer"]] == "ucminf") {
        vcov <- MASS::ginv(model[["hessian"]])
      } else {
        vcov <- MASS::ginv(-model[["hessian"]])
      }
      colnames(vcov) <- names(model[["param_final"]])
      rownames(vcov) <- names(model[["param_final"]])
      vcov
      
    }, error = function(e) {
      cli::cli_alert_danger("Failed to calculate VCOV matrix.")
      return(NA)
    })
    
    # Calculate the robust variance-covariance matrix
    if (estim_opt[["robust_vcov"]] && !anyNA(model[["vcov"]])) {
      model[["gradient_obs"]] <- numDeriv::jacobian(log_lik,
                                                    model[["param_final"]],
                                                    param_fixed = param_fixed,
                                                    workers = workers,
                                                    ll = ll, 
                                                    return_sum = FALSE,
                                                    pb = NULL,
                                                    method = "simple")
      
      bread <- model[["vcov"]] * n_id
      bread[is.na(bread)] <- 0
      
      meat <- (crossprod(model[["gradient_obs"]]) / n_id) * (n_id / (n_id - n_par))
      meat[is.na(meat)] <- 0
      
      model[["robust_vcov"]] <- (bread %*% meat %*% bread) / n_id
      
    } else {
      model[["robust_vcov"]] <- NA
    }
    
    
    time_diff <- Sys.time() - time_start_vcov
    cli::cli_alert_info(paste("VCOV calculation took",
                              round(time_diff, 2),
                              attr(time_diff, "units"),
                              sep = " "))
    
    # CALCULATE CONVERGENCE CRITERIA AND MODEL DIAGNOSTICS ----
    if (!anyNA(model[["vcov"]])) {
      gradient <- model[["gradient"]]
      vcov <- model[["vcov"]]
      model[["convergence_criteria"]] <- t(gradient) %*% vcov %*% gradient
    }
    
  } else {
    model[["hessian"]] <- NA
    model[["vcov"]] <- NA
  }
  
  # Calculate ll_0
  cli::cli_h2("Wrapping up")
  ll_0 <- tryCatch({
    ll_0_tmp <- log_lik(model[["param_final"]] * 0,
                        param_fixed = param_fixed,
                        workers = workers,
                        ll =ll,
                        return_sum = TRUE,
                        pb = NULL)
    
    if (estim_opt[["optimizer"]] %in% c("ucminf")) {
      -ll_0_tmp
      
    } else {
      ll_0_tmp
    }
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to calculate 'll(0)'")
    return(NA)
  })
  
  ll_2 <- -2L * model[["ll"]]
  model[["n_obs"]] <- n_obs
  model[["ll_0"]] <- ll_0
  model[["adj_rho_sqrd"]] <- (1L - ((model[["ll"]] - n_par) / (ll_0)))
  model[["aic"]] <- ll_2 + (2L * n_par)
  model[["aic3"]] <- ll_2 + (3L * n_par)
  model[["caic"]] <- ll_2 + (n_par * (log(n_obs) + 1L))
  model[["caic_star"]] <- ll_2 + (n_par * (log((n_obs + 2L) / 24L) + 1L ))
  model[["ht_aic"]] <- ll_2 + (2L * n_par) + (((2L * (n_par + 1L)) * (n_par + 2L))/(n_obs - n_par - 2L))
  model[["bic"]] <- ll_2 + (n_par * log(n_obs))
  model[["bic_star"]] <- ll_2 + (n_par * (log((n_obs + 2L) / 24L)))
  model[["dbic"]] <- ll_2 + (n_par * (log(n_obs) - log(2L * pi)))
  model[["hqic"]] <- ll_2 + (2L * (n_par * (log(log(n_obs)))))
  
  # WRAP UP AND RETURN MODEL OBJECT ----
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


#' Estimate the model using the maxLik package
#' 
#' The function is a convenient wrapper around \code{\link{maxLik}} and returns
#' a custom model object. 
#'
#' @param log_lik A log likelihood expression
#' @param param_free A named vector of freely estimated parameters
#' @param param_fixed A named vector of parameters to keep fixed at their
#' starting values during estimation
#' @param workers A list of workers or NA if single core
#' @param ll The 'raw' log-likelihood function
#' @param return_sum A boolean equal to TRUE if the log-likelihood function
#' shoudl return the sum of individual contributions. Default is FALSE.
#' @param pb A progress bar environment from the 'progress' package
#' @param estim_opt A list of estimation options
#'
#'
#' @return A custom model object with the results fo the estimation.
estimate_maxlik <- function(log_lik,
                            param_free,
                            param_fixed,
                            workers,
                            ll,
                            return_sum,
                            pb,
                            estim_opt) {
  
  model_obj <- tryCatch({
    maxLik::maxLik(log_lik,
                   start = param_free,
                   param_fixed = param_fixed,
                   workers = workers,
                   ll = ll,
                   return_sum = return_sum,
                   pb = pb,
                   method = estim_opt[["method"]],
                   finalHessian = FALSE,
                   tol = estim_opt[["tol"]],
                   gradtol = estim_opt[["gradtol"]],
                   reltol = estim_opt[["reltol"]], 
                   steptol = estim_opt[["steptol"]],
                   print.level = estim_opt[["print_level"]], 
                   iterlim = estim_opt[["iterlim"]])
  }, error = function(e) {
    return(
      list(
        code = 999
      )
    )
  })
  
  model <- list()
  model[["ll"]] <- model_obj[["maximum"]]
  model[["param_final"]] <- model_obj[["estimate"]]
  model[["iterations"]] <- model_obj[["iterations"]]
  model[["gradient"]] <- model_obj[["gradient"]]
  model[["gradient_obs"]] <- model_obj[["gradientObs"]]
  model[["message"]] <- model_obj[["message"]]
  
  if (estim_opt[["method"]] == "BFGS" && model_obj[["code"]] %in% c(0) ||
      estim_opt[["method"]] == "BHHH" && model_obj[["code"]] %in% c(2, 8) ||
      estim_opt[["method"]] == "NR" && model_obj[["code"]] %in% c(0, 1, 2)) {
    model[["converged"]] <- TRUE
    
  }   else {
    model[["converged"]] <- FALSE
  }
  
  return(model)
}

#' Estimate the model using the 'ucminf' package
#' 
#' The function is a convenient wrapper around \code{\link{ucminf}} and returns
#' a custom model object. 
#' 
#' @inheritParams estimate_maxlik
#' 
#' @return A custom model object with the results of the estimation
estimate_ucminf <- function(log_lik,
                            param_free,
                            param_fixed,
                            workers,
                            ll,
                            return_sum,
                            pb,
                            estim_opt) {
  model_obj <- tryCatch({
    ucminf::ucminf(par = param_free,
                   fn = log_lik,
                   hessian = 0,
                   param_fixed = param_fixed,
                   workers = workers,
                   ll = ll,
                   return_sum = return_sum,
                   pb = pb)
  }, error = function(e) {
    return(
      list(
        code = 999
      )
    )
  })
  
  
  # Added a minus to make the fit calculations correct
  model <- list()
  model[["ll"]] <- -model_obj[["value"]]
  model[["param_final"]] <- model_obj[["par"]]
  model[["message"]] <- model_obj[["message"]]
  model[["gradient"]] <- numDeriv::grad(log_lik,
                                        model[["param_final"]],
                                        param_fixed = param_fixed,
                                        workers = workers,
                                        ll = ll,
                                        return_sum = TRUE,
                                        pb = NULL)
  
  if (model_obj[["convergence"]] %in% c(1, 2, 3, 4)) {
    model[["converged"]] <- TRUE
  } else {
    model[["converged"]] <- FALSE
  }
  
  return(model)
}
