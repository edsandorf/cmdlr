#' Function to estimate a choice model using cmdlR
#' 
#' This is the main function used to estimate choice models. The function runs
#' a set of validation tests to check that all options are correctly specified. 
#' A correctly specified option only means that the model will run, it does not 
#' mean that the model specification is correct or that the combination of 
#' options are correct from a statistical point of view. 
#' 
#' If all checks are passed, the function will set up the estimation environment
#' and define correct and useful wrapper functions. This includes creating and
#' preparing workers in the context of parallel estimation.
#' 
#' Once the estimation environment is set up, the model will be estimated using
#' the options specified in \code{estim_opt()}. 
#'
#' @param ll A log likelihood function. If \code{estim_opt$optimizer = 'ucminf'},
#' then the log likelihood function should return the negative log likelihood
#' value.
#' 
#' @param db A \code{data.frame()} or \code{tibble()} with the data to be used in
#' estimation. 
#' 
#' @param estim_opt A list of options for the estimation procedure such as 
#' type of optimizer, approximation routines, tolerance levels etc. 
#' 
#' @param model_opt A list of model options including random parameters, draws,
#' fixed and non-fixed parameters during estimation, starting value search 
#' procedures etc. 
#' 
#' @param save_opt List of options for storing the outputs including which
#' model outputs to store, how to store them and where to store them.
#' 
#' @param debug A boolean indicating whether the code is run in debug mode. If
#' TRUE, the code will not estimate the models, but set the number of cores to 1
#' and return the estimation environment. Defaults to FALSE.
#' 
#' @return Returns a cmdlR model object. This is a list containing the following 
#' elements: 
#' 
#' @examples
#' \dontrun{
#'   # See /examples for how to use
#'   estimate(ll, db, estim_opt, model_opt, save_opt, debug = FALSE)
#' }
#' 
#' @export

estimate <- function(ll, db, estim_opt, model_opt, save_opt, debug = FALSE) {
  # INITIAL PREPARATIONS ----
  time_start <- Sys.time()
  message(blue$bold(symbol$info), bold(paste0("  Model estimation started: ", time_start, "\n")))
  
  # If we are in debug mode, make sure that we set up the estimation environment
  # for a single core only. The returned environment can be used to run e.g.
  # the analyze_choices() outside of estimate()
  if (debug) estim_opt$cores <- 1
  
  # VALIDATE OPTIONS ----
  message(blue$bold(symbol$info), bold("  Validating options"))
  time_start_validate <- Sys.time()
  
  # Estimation options
  estim_opt <- validate_estim_opt(estim_opt)
  
  # Model options
  model_opt <- validate_model_opt(model_opt, db)
  
  # Save options
  save_opt <- validate_save_opt(save_opt)
  
  # Print section time use
  time_diff <- Sys.time() - time_start_validate
  message(blue$bold(symbol$info), paste0("  Validating options took ",
                                         round(time_diff, 2), " ",
                                         attr(time_diff, "units"), "\n"))
  
  # PREPARE FOR ESTIMATION ----
  message(blue$bold(symbol$info), bold("  Preparing for estimation"))
  time_start_prepare <- Sys.time()
  
  # Data
  db <- prepare_data(db, estim_opt, model_opt)
  
  # Alternative availability
  model_opt$alt_avail <- lapply(model_opt$alt_avail, function(x) {
    if (x == 1) {
      rep(1, nrow(db))
    } else {
      db[[x]]
    }
  })
  
  # Analyze choices if explanators are present
  if (is.null(model_opt$choice_analysis_explanators)) {
    choice_analysis <- NULL
  } else {
    choice_analysis <- analyze_choices(db, model_opt)
  }
  
  # Draws
  if (model_opt$mixing) {
    draws <- prepare_draws(db, estim_opt, model_opt)
  } else {
    draws <- NULL
  }
  
  # Workers
  if (estim_opt$cores > 1) {
    # Create the cluster of workers and add stopCluster to on.exit()
    workers <- parallel::makeCluster(estim_opt$cores, type = "PSOCK")
    on.exit(parallel::stopCluster(workers), add = TRUE)
    
    # Prepare the workers
    prepare_workers(db, draws, workers, estim_opt, model_opt, save_opt)
    
  } else {
    # Create the estimation environment
    estim_env <- rlang::env()
    
    # Define indexes
    index_list <- list(
      N = length(unique(db[[model_opt[["id"]]]])),
      S = length(unique(db[[model_opt[["ct"]]]])),
      J = length(model_opt$alt_avail),
      alt_avail = model_opt$alt_avail,
      choice_var = db[[model_opt$choice]]
    )
    list2env(index_list, envir = estim_env)
    
    # Add the data to the estimation environment
    list2env(as.list(db), envir = estim_env)
    
    # Add the draws to the estimation environment
    if (model_opt$mixing) {
      list2env(as.list(draws), envir = estim_env)
    }
    workers <- NULL
    
    # Return the estimation environment and stop execution if in debug mode
    if (debug) return(estim_env)
  }
  
  # Prepare the log-likelihood function
  ll_func <- prepare_log_lik(ll, estim_env, model_opt, workers)
  
  ll_func_sum <- function(param) {
    sum(ll_func(param))
  }
  
  # Prepare the numerical gradient
  num_grad <- prepare_num_grad(ll, estim_env, workers)
  
  # Print section time use
  time_diff <- Sys.time() - time_start_prepare
  message(blue$bold(symbol$info), paste0("  Preparing for estimation took ",
                                         round(time_diff, 2), " ",
                                         attr(time_diff, "units"), "\n"))
  
  # SEARCH FOR START VALUES ----
  if (estim_opt$search_start) {
    time_start_search <- Sys.time()
    
    start_values <- search_start_values(ll_func, estim_env, estim_opt, model_opt)
    model_opt$param <- as.list(start_values[1, , drop = TRUE][-ncol(start_values)])
    
    time_diff <- Sys.time() - time_start_search
    message(blue$bold(symbol$info), paste0("  Search for starting values took ",
                                           round(time_diff, 2), " ",
                                           attr(time_diff, "units"), "\n"))
    
  } else {
    start_values <- NULL
  }
  
  # ESTIMATE THE MODEL ----
  message(blue$bold(symbol$info), bold("  Estimating the model"))
  time_start_estimate <- Sys.time()
  converged <- FALSE
  
  # Create the model object
  model <- list()
  model[["name"]] <- save_opt$name
  model[["description"]] <- save_opt$description
  model[["method"]] <- tolower(estim_opt$method)
  model[["optimizer"]] <- tolower(estim_opt$optimizer)
  model[["cores"]] <- estim_opt$cores
  model[["R"]] <- model_opt$R
  model[["draws_type"]] <- model_opt$draws_type
  model[["time_start"]] <- time_start
  model[["nobs"]] <- model_opt$nobs
  model[["choice_analysis"]] <- choice_analysis
  
  # Prepare the starting parameters by separating the free and fixed parameters
  # into two vectors - see the 'apollo' package for details. 
  param <- unlist(model_opt$param)
  param_free <- param[!(names(param) %in% model_opt$fixed)]
  param_fixed <- param[model_opt$fixed]
  
  # Add the starting parameters and fixed parameters to the model object
  model[["param_start"]] <- param
  model[["starting_values"]] <- start_values
  model[["param_fixed"]] <- param_fixed
  
  # Other
  N <- model_opt$N
  K <- length(param_free)
  
  # Estimate the model using the 'maxLik' package
  if (tolower(estim_opt$optimizer) == "maxlik") {
    model_obj <- tryCatch({
      maxLik::maxLik(ll_func,
                     start = param_free,
                     method = estim_opt$method,
                     finalHessian = FALSE,
                     tol = estim_opt$tol, gradtol = estim_opt$gradtol,
                     reltol = estim_opt$reltol, 
                     steptol = estim_opt$steptol,
                     print.level = estim_opt$print_level, 
                     iterlim = estim_opt$iterlim)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(model_obj)) {
      return(model)
    }
    
    model[["ll"]] <- model_obj$maximum
    model[["param_final"]] <- model_obj$estimate
    model[["iterations"]] <- model_obj$iterations
    model[["gradient"]] <- model_obj$gradient
    model[["gradient_obs"]] <- model_obj$gradientObs
    model[["message"]] <- model_obj$message
    
    if (tolower(estim_opt$method) == "bfgs" && model_obj$code %in% c(0)) converged <- TRUE 
    if (tolower(estim_opt$method) == "bhhh" && model_obj$code %in% c(2, 8)) converged <- TRUE 
    if (tolower(estim_opt$method) == "nr" && model_obj$code %in% c(0, 1, 2)) converged <- TRUE 
  }
  
  # Estimate the model using the 'ucminf' package
  if (tolower(estim_opt$optimizer) == "ucminf") {
    model_obj <- tryCatch({
      ucminf::ucminf(par = param_free,
                     fn = ll_func_sum,
                     hessian = 0)
    }, error = function(e) {
      return(model)
    })

    
    # Added a minus to make the fit calculations correct
    model[["ll"]] <- -model_obj$value
    model[["param_final"]] <- model_obj$par
    model[["message"]] <- model_obj$message
    model[["gradient"]] <- numDeriv::grad(ll_func_sum, model[["param_final"]])
    
    if (model_obj$convergence %in% c(1, 2, 3, 4)) converged <- TRUE
  }
  
  if (estim_opt$iterlim %in% c(0, 1)) {
    converged <- TRUE
  }
  
  if (!converged) {
    stop("Model failed to converge. Estimation unsuccessful.\n")
  }
  
  # Add converged boolean to model object
  model[["converged"]] <- converged
  
  # Add the log-likelihood values and attributes to the model object ----
  model[["ll_values"]] <- ll_func(model[["param_final"]])
  
  # Print section time use
  time_diff <- Sys.time() - time_start_estimate
  message(blue$bold(symbol$info), paste0("  Model estimation took ",
                                         round(time_diff, 2), " ",
                                         attr(time_diff, "units"), "\n"))
  
  # Check if we are calculating the Hessian matrix
  if (estim_opt$calculate_hessian) {
    # CALCULATE THE HESSIAN MATRIX ----
    time_start_hessian <- Sys.time()
    
    # Define the progress bar
    K <- length(model[["param_final"]])
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :elapsed",
      total = 2 + 8 * (K * (K + 1) / 2),
      clear = FALSE, 
      width = 80
    )
    
    # Define the wrapper function
    ll_func_pb <- function(param) {
      pb$tick()
      sum(ll_func(param))
    }
    
    # Try and catch if the Hessian cannot be calculated 
    model[["hessian"]] <- tryCatch({
      message(blue$bold(symbol$info), bold("  Calculating the Hessian matrix"))
      hessian <- numDeriv::hessian(func = ll_func_pb, x = model[["param_final"]])
      colnames(hessian) <- names(model[["param_final"]])
      rownames(hessian) <- names(model[["param_final"]])
      hessian
    }, error = function(e) {
      NA
    })
    
    # If the Hessian calculation failed, try calculating it using the maxLik package
    if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
      # Print messages to console
      message(red$bold(symbol$cross), "  Hessian calculation using the \'numDeriv\' package.\n")
      message(blue$bold(symbol$info), "  Trying to calculate Hessian using the \'maxLik\' package.\n")
      
      # Reset the progress bar
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent :elapsed",
        total = 2 + 8 * (K * (K + 1) / 2),
        clear = FALSE, 
        width = 80
      )
      
      # Try calculating the hessian using the maxLik package
      model[["hessian"]] <- tryCatch({
        hessian <- maxLik::maxLik(ll_func_pb,
                                  start = model[["param_final"]],
                                  print.level = 0,
                                  finalHessian = TRUE, method = estim_opt$method,
                                  iterlim = 2)$hessian
        colnames(hessian) <- names(model[["param_final"]])
        rownames(hessian) <- names(model[["param_final"]])
        hessian
      }, error = function(e) {
        NA
      })
    }
    
    # If the Hessian still cannot be calculated end and return the model object
    if (is.na(model[["hessian"]]) || anyNA(model[["hessian"]])) {
      message(red$bold(symbol$cross), "  Hessian calculation failed or contains NA. Returning only some model information.\n")
      time_end <- Sys.time()
      model[["time_end"]] <- time_end
      return(model)
    }
    
    model_obj[["hessian"]] <- model[["hessian"]]
    message(green$bold(symbol$tick), "  Hessian calculated successfully.")
    
    # Print section time use
    time_diff <- Sys.time() - time_start_hessian
    message(blue$bold(symbol$info), paste0("  Hessian calculation took ",
                                           round(time_diff, 2), " ",
                                           attr(time_diff, "units"), "\n"))
    
    # CALCULATE THE VCOV MATRIX ----
    message(blue$bold(symbol$info), bold("  Calculating the variance-covariance matrices"))
    time_start_vcov <- Sys.time()
    
    # Standard
    model[["vcov"]] <- tryCatch({
      if (estim_opt$optimizer == "ucminf") {
        vcov <- MASS::ginv(model[["hessian"]])
      } else {
        vcov <- MASS::ginv(-model[["hessian"]])
      }
      colnames(vcov) <- names(model[["param_final"]])
      rownames(vcov) <- names(model[["param_final"]])
      vcov
    }, error = function(e) {
      message(red$bold(symbol$cross), "  Failed to calculate the variance covariance matrix.\n")
      NULL
    })
    
    # Calculate the robust variance-covariance matrix
    if (estim_opt$robust_vcov && !is.null(model[["vcov"]])) {
      model[["gradient_obs"]] <- numDeriv::jacobian(ll_func, model[["param_final"]], method = "simple")
      
      bread <- model[["vcov"]] * N
      bread[is.na(bread)] <- 0
      
      meat <- (crossprod(model[["gradient_obs"]]) / N) * (N / (N - K))
      meat[is.na(meat)] <- 0
      
      model[["robust_vcov"]] <- (bread %*% meat %*% bread) / N
      
    } else {
      model[["robust_vcov"]] <- NULL
    }
    
    time_diff <- Sys.time() - time_start_vcov
    message(blue$bold(symbol$info), paste0("  Variance-covariance calculations took ",
                                           round(time_diff, 2), " ",
                                           attr(time_diff, "units"), "\n"))
    
    # CALCULATE CONVERGENCE CRITERIA AND MODEL DIAGNOSTICS ----
    if (!is.null(model[["vcov"]])) {
      model[["convergence_criteria"]] <- t(model[["gradient"]]) %*% model[["vcov"]] %*% model[["gradient"]]
    }
    
  } else {
    model[["hessian"]] <- NULL
    model[["vcov"]] <- NULL
  }
    
  ll_0 <- tryCatch({
    ll_0_tmp <- sum(ll_func(model[["param_final"]] * 0))
    if (tolower(estim_opt$optimizer) %in% c("ucminf")) {
      -ll_0_tmp
    } else {
      ll_0_tmp
    }
  }, error = function(e) {
    cat(red$bold(symbol$cross), "  Failed to calculate 'll_0'. Replacing with NA.\n")
    return(NA)
  })
  
  ll <- model[["ll"]]
  nobs <- model[["nobs"]]
  model[["ll_0"]] <- ll_0
  model[["adj_rho_sqrd"]] <- (1L - ((ll - K) / (ll_0)))
  model[["aic"]] <- ((-2L * ll) + (2L * K) )
  model[["aic3"]] <- ((-2L * ll) + (3L * K))
  model[["caic"]] <- ((-2L * ll) + (K * (log(nobs) + 1L)))
  model[["caic_star"]] <- ((-2L * ll) + (K * (log((nobs + 2L) / 24L) + 1L )))
  model[["ht_aic"]] <- ((-2L * ll) + (2L * K) + (((2L * (K + 1L)) * (K + 2L))/(nobs - K - 2L)))
  model[["bic"]] <- ((-2L * ll) + (K * log(nobs)))
  model[["bic_star"]] <- ((-2L * ll) + (K * (log((nobs + 2L) / 24L))))
  model[["dbic"]] <- ((-2L * ll) + (K * (log(nobs) - log(2L * pi))))
  model[["hqic"]] <- ((-2L * ll) + (2L * (K * (log(log(nobs))))))
  
  # WRAP UP AND RETURN MODEL OBJECT ----
  time_end <- Sys.time()
  message(green$bold(symbol$tick), bold(paste0("  Estimation completed on ", time_end, "\n")))
  model[["time_end"]] <- time_end
  
  # Explicit return
  return(model)
}
