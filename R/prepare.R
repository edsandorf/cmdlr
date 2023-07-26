#' Prepare the estimation environment
#' 
#' The function prepares the data, draws and alternative availabilites for 
#' estimation. All model components are placed in an estimation environment
#' which provides the context for evaluating the log likelihood function.
#'
#' @param ll A user supplied log-likelihood function
#' @param db A \code{data.frame()} or \code{tibble()} containing the data
#' @param model_options A list of user supplied model options. This list is 
#' validated within the function. See \code{\link{validate}} for details.
#' @param control A list of control options
#' @param check_data A boolean equal to TRUE if we should run data checks and 
#' pad the data with NA.
#' @param debug A boolean equal to TRUE if you are debugging the log likelihood
#' function. It forces the number of cores to 1. Then you can simply
#' \code{\link{attach}} the estimation environment and the list of parameters
#' to run the log likelihood function line by line. The default is FALSE.
#' @param ... Additional named arguments that are added to the estimation
#' environment. If cores > 1 the arguments are added to each individual 
#' estimation environment. No attempts at splitting across cores. This is
#' useful if you need to pass e.g. a restriction matrix or other variables 
#' to the estimation environment so that they can be called by name/reference 
#' within the log-likelihood function. 
#' 
#' @return The estimation environment or a list of estimation environments if
#' cores > 1. 
#' 
#' @export
prepare <- function(ll,
                    db, 
                    model_options,
                    control = NULL,
                    check_data = TRUE,
                    debug = FALSE,
                    ...) {

  time_start <- Sys.time()
  cli::cli_h1("Preparing for estimation")
  
  # Check that the function inputs are of the correct type
  stopifnot("'db' must be a data.frame or tibble"= 
              is.data.frame(db) || tibble::is_tibble(db))
  
  stopifnot("'model_options' must be a list"= 
              is.list(model_options))
  
  stopifnot("'control' must be a list or NULL"= 
              is.list(control) || is.null(control))
  
  stopifnot("'check_data' must be TRUE/FALSE"= 
              is.logical(check_data))
  
  stopifnot("'debug' must be TRUE/FALSE"= 
              is.logical(debug))
  
  
  # Validate model options ----
  cli::cli_h2("Validating model options")
  model_options <- validate(model_options, names(db))
  cli::cli_alert_success("Model options validated and defaults set")
  
  # Core check ----
  # Just get the defaults to set cores
  cores <- set_controls(control = control)[["cores"]]
  
  if (debug) {
    cores <- 1
  
  }
  
  # Extract variables from the model options ----
  str_id <- model_options[["id"]]
  str_ct <- model_options[["ct"]]
  str_choice <- model_options[["choice"]]
  str_param_fixed <- model_options[["fixed"]]
  n_draws <- model_options[["n_draws"]]
  draws_type <- model_options[["draws_type"]]
  rpar <- model_options[["rpar"]]
  seed <- model_options[["seed"]]
  n_id <- length(unique(db[[str_id]]))
  n_ct <- length(unique(db[[str_ct]]))
  n_obs <- nrow(db)
  param <- model_options[["param"]]
  param_fixed <- param[str_param_fixed]
  
  # Prepare the data ----
  db <- prepare_data(db, str_id, str_ct, n_id, n_ct, cores, check_data)
  
  # Prepare alternative availability ----
  alt_avail <- prepare_alt_avail(model_options[["alt_avail"]], db)
  
  # Subset the data *after* alt_avail since alt_avail won't be part of the LL
  db <- subset_data(ll, db, str_id, str_ct, str_choice)
  
  # Prepare the draws ----
  if (model_options[["mixing"]]) {
    draws <- prepare_draws(n_id, n_ct, n_draws, draws_type, rpar, seed)
    
  } else {
    draws <- NULL
    
  }
  
  # Prepare estim_env() ----
  estim_env <- prepare_estimation_environment(db,
                                              alt_avail,
                                              draws,
                                              cores,
                                              str_id,
                                              str_ct,
                                              str_choice,
                                              n_obs,
                                              param_fixed)

  # Return the list of prepared objects
  cli::cli_alert_success("All preparations complete")
  
  time_diff <- Sys.time() - time_start
  cli::cli_alert_info(paste("Preparing for estimation took",
                            round(time_diff, 2),
                            attr(time_diff, "units"),
                            sep = " "))
  
  return(estim_env)
}

#' Prepare the estimation environment
#'
#' The estimation environment provides the context for evaluating the
#' log-likelihood function. Creating an environment allow us to call parameters,
#' variables and indexes directly without calls to \code{\link{attach}} and 
#' \code{\link{detach}}. 
#' 
#' The use of an estimation environment also makes debugging easy because it is
#' possible attach the environment directly to run through the
#' log-likelihood function line-by-line. To efficiently debug, it is 
#' important that the number of cores is set to 1.
#' 
#' To run the model on subsets of the data, simply pass a subset of the data
#' using the db argument. 
#'
#' @inheritParams prepare
#' @param alt_avail A list of alternative availabilites supplied by the user
#' @param draws A list of matrixes with draws for each random parameter in the
#' model
#' @param cores An integer indicating the number of cores to use in estimation
#' @param str_id A character string giving the name of the id variable
#' @param str_ct A character string giving the name of the ct variable
#' @param str_choice A character string giving the name of the choice variable
#' @param n_obs An integer giving the number of observations
#' @param param_fixed A list of fixed parameters to add to environment
#' @param ... Additional objects passed on to the estimation environment. NOTE:
#' these are NOT split by core or respondents, but exported in its entirety.
#' 
#' @return An environment or list of environments providing the context for 
#' evaluating the log-likelihood function. 
prepare_estimation_environment <- function(db,
                                           alt_avail,
                                           draws,
                                           cores,
                                           str_id,
                                           str_ct,
                                           str_choice,
                                           n_obs,
                                           param_fixed, 
                                           ...) {
  # Define useful variables
  n_alt <- length(alt_avail)
  
  # Check if we are using more than one core
  if (cores > 1) {
    db <- split_data(db, str_id, cores)
    
    split_idx <- get_split_index(db, cores)
    
    alt_avail <- lapply(split_idx, function(idx) {
      lapply(alt_avail, function(x) {
        x[idx]
      })
    })
    
    draws <- lapply(split_idx, function(idx) {
      lapply(draws, function(x) {
        x[idx, , drop = FALSE]
      })
    })
    
    estim_env <- lapply(seq_along(db), function(i) {
      estim_env <- rlang::env()
      
      list2env(
        c(
          list(
            n_id = length(unique(db[[i]][[str_id]])),
            n_ct = length(unique(db[[i]][[str_ct]])),
            n_alt = n_alt,
            n_obs = n_obs,
            choice_var = db[[i]][[str_choice]],
            alt_avail = alt_avail[[i]],
            db_names = names(db[[i]])
          ),
          as.list(db[[i]]),
          as.list(draws[[i]]),
          as.list(param_fixed),
          list(...)
        ), envir = estim_env
      )
    })
    
    
  } else {
    estim_env <- rlang::env()
    
    list2env(
      c(
        list(
          n_id = length(unique(db[[str_id]])),
          n_ct = length(unique(db[[str_ct]])),
          n_alt = n_alt,
          n_obs = n_obs,
          choice_var = db[[str_choice]],
          alt_avail = alt_avail,
          db_names = names(db)
        ),
        as.list(db), 
        as.list(draws),
        as.list(param_fixed),
        list(...)
      ), envir = estim_env
    )
    
  }
  
  # Return the (list of) estimation environment(s)
  cli::cli_alert_success("Estimation environment")
  
  return(estim_env)
}

#' Prepare the data for estimation
#'
#' The function checks that the data is supplied as a data.frame or tibble and
#' that it contains an equal number of choice observations per individual. 
#' 
#' NOTE: While padding allows the use of fast matrix operations, it may come
#' at the cost of disproportionate memory use for large datasets with many
#' observations and variables.
#' 
#' @param check_data A boolean eqaul to TRUE if the code should skip the 
#' data checks on number of choice tasks per individual. 
#' @inheritParams prepare_estimation_environment
#' @inheritParams prepare_draws
#' 
#' @return A \code{\link{data.frame}} padded with NA 
prepare_data <- function(db,
                         str_id,
                         str_ct,
                         n_id,
                         n_ct,
                         cores,
                         check_data) {
  
  # Define variables and indexes

  unique_id <- unique(db[[str_id]])
  
  # Check choice tasks
  if (((n_id * n_ct) != nrow(db)) & check_data) {
    cli::cli_alert_warning(
      "Unequal number of choices per respondent. Padding the data with NA."
    )
    
    # Create a data.frame with only a choice task indicator
    choice_tasks <- data.frame(ct_tmp = seq_len(n_ct))
    names(choice_tasks) <- str_ct
    
    db <- lapply(unique_id, function(i) {
      db_tmp <- db[db[, str_id] == i, ]
      
      # Pad the data if rows differ
      if (nrow(db_tmp) != n_ct) {
        db_tmp <- merge(choice_tasks, db_tmp, by = str_ct, all.x = TRUE)
        db_tmp[, str_id] <- i
      }
      
      return(db_tmp)
    })
    
    
    db <- do.call(rbind, db)
  }
  
  # Return the manipulated and checked data
  cli::cli_alert_success("Data")
  
  return(db)
}

#' Prepare the alternative availabilities
#' 
#' Sets up the list of alternative availabilities. If alternative availability
#' is an integer equal to 1, i.e. always available, then repeats it equal to 
#' the number of rows in the (padded) data, if not includes the actual 
#' availability vector. 
#' 
#' @inheritParams prepare_estimation_environment
#' 
#' @return A list of alternative availabilities
prepare_alt_avail <- function(alt_avail,
                              db) {
  alt_avail <- lapply(alt_avail, function(x) {
    if (x == 1L) {
      rep(1L, nrow(db))
    } else {
      db[[x]]
    }
  })
  
  return(alt_avail)
}

#' Prepares the draws
#' 
#' In a model using simulation, we need to prepare a set of random draws to 
#' use when simulating/approximating the distributions/integrals. The function
#' is a convenient wrapper around the function \code{\link{make_draws}}.
#' 
#' Depending on the model options, the function will transform the supplied
#' uniform draws to either standard uniform, standard normal or standard
#' triangular. It is up to the user to specify the correct distributions in the
#' log-likelihood function.
#' 
#' @param n_id An integer giving the nubmer of respondents
#' @param n_ct An integer giving the number of choice situations
#' @param n_draws An integer giving the number of draws per individual 
#' @param type A character string giving the type of draws to use
#' @param rpar A named list giving the type of distribution
#' @param seed An integer giving the seed to change the scrambling of the sobol
#' sequence
#' 
#' @return A list of matrixes matching 'rpar' where each matrix has the 
#' dimensions n_id*n_ct x n_draws  
prepare_draws <- function(n_id,
                          n_ct,
                          n_draws, 
                          type,
                          rpar,
                          seed) {
  # Extract the relevant information from model_options() ----
  n_rpar <- length(rpar)
  
  # Make the draws and repeat rows equal to S - N*S x R
  draws <- make_draws(n_id, n_draws, n_rpar, seed, type)
  if (n_rpar == 1) {
    draws <- matrix(draws, ncol = 1L)
  }
  
  # Convert the draws to normal, uniform, triangular ----
  for (i in seq_along(rpar)) {
    if (rpar[[i]] == "normal") {
      draws[, i] <- stats::qnorm(draws[, i])
    }
    
    if (rpar[[i]] == "triangular") {
      index <- as.integer(draws[, i] < 0.5)
      draws[, i] <- index * (sqrt(2 * draws[, i]) - 1) + (1 - index) * (1 - sqrt(2 * (1 - draws[, i])))
    }
  }
  
  # Turn into a list of matrices with the correct dimensions
  draws <- lapply(as.list(as.data.frame(draws)), function(x) {
    matrix(rep(matrix(x, ncol = n_draws, byrow = TRUE), each = n_ct), ncol = n_draws)
  })
  names(draws) <- names(rpar)
  
  # Return the list of draws
  return(draws)
}

#' Prepares the log likelihood function
#'
#' The function takes the user supplied log-likelihood function and creates
#' convenient wrappers that ensure the function can be evaluated both using a
#' single core and multiple cores. 
#' 
#' @inheritParams estimate
#' @param estim_env An estimation environment returned by 
#' \code{\link{prepare_estimation_environment}}
#' @param workers A list of workers created using the parallel package
#' 
#' @return The estimation-ready log likelihood function
prepare_log_lik <- function(ll,
                            estim_env,
                            workers) {
  
  # The inner part of the estimation algorithm
  log_lik_inner <- function(param, ll) {
    list2env(as.list(param), envir = estim_env)

    return(
      eval(body(ll), estim_env)
    )
  }
  
  # Set the environment for the inner function
  if (!anyNA(workers)) {
    environment(log_lik_inner) <- environment(prepare_log_lik)
  }
  
  log_lik_outer <- function(param_free, 
                            parallel = !anyNA(workers),
                            return_log = FALSE,
                            return_sum = FALSE,
                            pb = NULL) {
    
    if (!is.null(pb)) {
      pb$tick()
    }
    
    if (parallel) {
      values <- parallel::clusterCall(cl = workers,
                                      fun = log_lik_inner,
                                      param = param_free,
                                      ll = ll)
      
      values <- do.call(c, values)      
      
    } else {
      values <- log_lik_inner(param = param_free, ll = ll)
      
    }
    
    # Make the necessary transformations of the return value
    if (return_log) {
      values <- log(values)
    }
    
    if (return_sum) {
      values <- sum(values)
    }
    
    return(values)
  }
  
  # Return the log likelihood function
  cli::cli_alert_success("Log-likelihood function")
  return(log_lik_outer)
}

#' Prepares the numerical gradient
#'
#' The optimization routines 'ucminf', 'nloptr' and 'trustOptim' requires the
#' user to supply a gradient. Writing an analytical gradient can be quite
#' cumbersome for very complex likelihood expressions. This function is a simple
#' wrapper around \code{numDeriv::grad()} and prepares a high-precision 
#' numerical gradient that can be supplied directly to the optimizers that 
#' require one. 

#' Note that a numerical gradient is slower in calculation and less precise than 
#' an analytical gradient.
#' 
#' @inheritParams prepare_log_lik
#' 
#' @return A high precision numerical gradient function
prepare_num_grad <- function(ll,
                             estim_env,
                             workers) {
  # Define the eval() wrapper for the numerical gradient
  num_grad_inner <- function(param, ll) {
    list2env(as.list(param), envir = estim_env)
    return(sum(eval(body(ll), estim_env)))
  }
  
  if (!anyNA(workers)) {
    environment(num_grad_inner) <- environment(prepare_num_grad)
  }
  
  # Define the numerical gradient
  num_grad_outer <- function(param_free,
                             param_fixed,
                             workers,
                             ll) {
    
    param <- c(param_free, param_fixed)
    
    if (anyNA(workers)) {
      values <- numDeriv::grad(num_grad_inner,
                               param, 
                               method = "Richardson",
                               ll = ll)
      
    } else {
      values <- parallel::clusterCall(
        cl = workers,
        fun = function(param, ll) {
          numDeriv::grad(num_grad_inner,
                         param = param,
                         method = "Richardson",
                         ll = ll)
        },
        param = param, 
        ll = ll
      )
      
      values <- do.call(rbind, values)
      values <- Rfast::colsums(values)
    }
    
    return(values)
  }
  
  # Return the numerical gradient
  cli::cli_alert_success("Numerical gradient")
  return(num_grad_outer)
}

