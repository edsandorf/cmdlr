#' Validate all options
#'
#' The function validates all user supplied lists of options and sets reasonable
#' default values for missing options. All checks are intentionally aggressive
#' and will throw an error rather than make assumptions about what the user
#' intended to do. 
#' 
#' @param input_estim_opt A named list of estimation options
#' @param input_model_opt A named list of model options
#' @param input_save_opt A named list of save options
#' @param db A data.frame with the data used for estimation 
#'  
#' @return Returns a list with three elements. 
#' 
#' @export
validate <- function(input_estim_opt, input_model_opt, input_save_opt, db) {
  cli::cli_h1("Validating options")
  
  list_of_options <- list(
    estim_opt = validate_estim_opt(input_estim_opt),
    model_opt = validate_model_opt(input_model_opt, db),
    save_opt = validate_save_opt(input_save_opt)
  )
  
  cli::cli_alert_success("All options validated and defaults set.")
  return(list_of_options)
}

#' Validates and sets default estimation options
#'
#' The function validates the user supplied list of estimation options and sets 
#' defaults for missing options. 
#' 
#' @inheritParams validate
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_estim_opt <- function(input_estim_opt) {
  # Set the missing to default values
  estim_opt <- list(
    tol = 1e-08,
    reltol = sqrt(.Machine$double.eps),
    gradtol = 1e-06,
    steptol = 1e-10,
    lambdatol = 1e-6,
    print_level = 0,
    iterlim = 500,
    cores = 1,
    calculate_hessian = TRUE,
    robust_vcov = TRUE,
    check_data = TRUE
  )
  
  # Replace the non-specified values with default values
  estim_opt[names(input_estim_opt)] <- input_estim_opt
  
  # Check that the optimizer and method is specified correctly
  if (is.null(estim_opt[["optimizer"]]) || is.null(estim_opt[["method"]])) {
    stop("Optimizer and method must be specified in estim_opt")
  } else {
    method <- estim_opt$method <- toupper(estim_opt$method)
    optimizer <- estim_opt$optimizer <- tolower(estim_opt$optimizer)
  }
  
  # Check that the specified optimizer is correct
  if (!(optimizer %in% c("maxlik", "ucminf"))) {
    stop("Optmizer must be 'maxlik' or 'ucminf'")
  } 
  
  # Check that the specified method is correct. 
  if (optimizer == "maxlik" && !(method %in% c("BFGS", "BHHH", "NR"))) {
    stop("Method must be either 'BFGS', 'BHHH' or 'NR' with 'maxlik'.")
  }
  
  if (optimizer == "ucminf" && !(method %in% "BFGS")) {
    stop("Method must be 'BFGS' with 'ucminf'")
  }
  
  # Check the number of cores
  if (estim_opt$cores > parallel::detectCores()) {
    stop("The number of specified cores exceed available.")
  }
  
  # Check that input values are logical
  stopifnot(is.logical(estim_opt[["calculate_hessian"]]))
  stopifnot(is.logical(estim_opt[["robust_vcov"]]))
  stopifnot(is.logical(estim_opt[["check_data"]]))
  
  # Return the validated list of estimation options
  cli::cli_alert_success("Estimation options")
  
  return(estim_opt)
}


#' Validates and sets default model options
#'
#' The function validates the user supplied list of model options and sets 
#' defaults for the missing options.
#'
#' @inheritParams validate
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_model_opt <- function(input_model_opt, db) {
  # Set the defaults ----
  model_opt <- list(
    id = NULL,
    ct = NULL,
    choice = NULL,
    alt_avail = NULL,
    mixing = FALSE,
    draws_type = "scrambled-sobol",
    n_draws = 10,
    fixed = c(""),
    param = NULL,
    rpar = NULL,
    seed = 123
  )
  
  # Replace the non-specified values with default values
  model_opt[names(input_model_opt)] <- input_model_opt
  str_db_names <- names(db)
    
  # Check general model options
  str_var <- model_opt[["id"]]
  if (is.null(str_var) || !(str_var %in% str_db_names)) {
    stop("'id' must be specified and exist in the data")
  }
  
  str_var <- model_opt[["ct"]]
  if (is.null(str_var) || !(str_var %in% str_db_names)) {
    stop("'ct' must be specified and exist in the data")
  }
  
  str_var <- model_opt[["choice"]]
  if (is.null(str_var) || !(str_var %in% str_db_names)) {
    stop("'choice' must be specified and exist in the data")
  }
  

  # Check alternative availability
  if (is.null(model_opt[["alt_avail"]])) {
    stop("Alternative availabilities must be specified.")
    
  } else {
    lapply(model_opt[["alt_avail"]], function(x) {
      if ((is.numeric(x) & length(x) == 1 & x == 1) | (x %in% str_db_names)) {
        NULL
      } else {
        stop("'alt_avail' must indicate a variable or be equal to 1")
      }
    })
  }
  
  # Check parameters
  param <- model_opt[["param"]]
  if (is.null(param) || !is.list(param)) {
    stop("'parma' must be a named list of parameters/starting values")
  } 
  
  # Check the fixed parameters
  fixed <- model_opt[["fixed"]]
  n_fixed <- length(fixed)
  
  if (n_fixed > 0 && !is.character(fixed)) {
    stop("'fixed' is a character vector with paramers to keep fixed")
  }
  
  if (length(unique(fixed)) < n_fixed) {
    stop("'fixed' contains duplicate elements")
  }
  
  if (!all(fixed %in% names(param))) {
    "Some 'fixed' parameters are not specified in 'param'"
  }
  
  # Check mixing options
  if (model_opt[["mixing"]]) {
    draws_type <- model_opt[["draws_type"]] <- tolower(model_opt$draws_type)
    if (!(draws_type %in% c("pseudo-random",
                            "mlhs",
                            "scrambled-halton",
                            "standard-halton",
                            "scrambled-sobol",
                            "standard-sobol"))) {
      stop("Unknown type of draws specified")
    }
    
    rpar <- model_opt[["rpar"]]
    if (is.null(rpar) || !is.list(rpar)) {
      stop("'rpar' must be a named list indicating the distributions")
    } else {
      rpar <- model_opt[["rpar"]] <- lapply(rpar, tolower)
    }
    
    if (!all(unlist(rpar) %in% c("normal",
                                 "triangular",
                                 "uniform"))) {
      stop("Unknown distribution type specified")
    }
  }
  
  # Return the validated list of model options
  cli::cli_alert_success("Model options")
  
  return(model_opt)
}

#' Validates and sets default save options
#'
#' The function validates the user supplied list of save options and sets 
#' defaults for the missing options.
#' 
#' The function is intended for internal use only.
#'  
#' @inheritParams validate
#' 
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_save_opt <- function(input_save_opt) {
  save_opt <- list(
    name = "A model without a name",
    description = "Does not contain a description",
    path = NULL,
    save_summary = FALSE,
    save_model_object = FALSE,
    save_hessian = FALSE,
    save_vcov = FALSE,
    save_param = FALSE,
    save_worker_info = FALSE
  )
  
  # Replace the non-specified values with default values
  save_opt[names(input_save_opt)] <- input_save_opt
  
  # Check output location
  if (is.null(save_opt[["path"]])) {
    cli::cli_alert_info(
      paste0("No output folder specified. Using default location:\n",
             file.path(getwd()))
    )
  }
  
  # Check that the input values are logical
  stopifnot(is.logical(save_opt[["save_summary"]]))
  stopifnot(is.logical(save_opt[["save_model_object"]]))
  stopifnot(is.logical(save_opt[["save_hessian"]]))
  stopifnot(is.logical(save_opt[["save_vcov"]]))
  stopifnot(is.logical(save_opt[["save_param"]]))
  stopifnot(is.logical(save_opt[["save_worker_info"]]))
  
  
  
  # Return the validated list of saving options
  cli::cli_alert_success("Save options")
  
  return(save_opt)
}
