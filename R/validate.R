#' Validate model options
#'
#' The function validates the user supplied list of model options and sets 
#' defaults for missing options. All checks are intentionally aggressive
#' and will throw an error rather than make assumptions about what the user
#' intended to do. 
#'
#' The function is run once inside of \code{\link{prepare}} to correctly set up 
#' the estimation environment. It is meant for internal use only and is not 
#' exported. 
#'
#' @param model_options A user supplied list of model options
#' @param db_names A character string of variable names in the data
#'  
#' @return A list of model options with missing input values replaced by 
#' default values 
validate <- function(model_options, db_names) {
  # Define a list of default options
  model_options_default <- list(
    name = "A model without a name",
    description = "Does not contain a description",
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
  model_options_default[names(model_options)] <- model_options
  
  # Check general model options
  str_var <- model_options_default[["id"]]
  if (is.null(str_var) || !(str_var %in% db_names)) {
    stop("'id' must be specified and exist in the data")
  }
  
  str_var <- model_options_default[["ct"]]
  if (is.null(str_var) || !(str_var %in% db_names)) {
    stop("'ct' must be specified and exist in the data")
  }
  
  str_var <- model_options_default[["choice"]]
  if (is.null(str_var) || !(str_var %in% db_names)) {
    stop("'choice' must be specified and exist in the data")
  }
  
  # Check alternative availability
  if (is.null(model_options_default[["alt_avail"]])) {
    stop("Alternative availabilities must be specified")
    
  } else {
    lapply(model_options_default[["alt_avail"]], function(x) {
      if ((is.numeric(x) & length(x) == 1 & x == 1) | (x %in% db_names)) {
        NULL
      } else {
        stop("'alt_avail' must indicate a variable or be equal to 1")
      }
    })
  }
  
  # Check parameters
  param <- model_options_default[["param"]]
  if (is.null(param) || !is.list(param)) {
    stop("'param' must be a named list of parameters")
  } 
  
  # Check the fixed parameters
  fixed <- model_options_default[["fixed"]]
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
  if (model_options_default[["mixing"]]) {
    draws_type <- model_options_default[["draws_type"]] <- tolower(model_options_default[["draws_type"]])
    if (!(draws_type %in% c("pseudo-random",
                            "mlhs",
                            "scrambled-halton",
                            "standard-halton",
                            "scrambled-sobol",
                            "standard-sobol"))) {
      stop("Unknown type of draws specified")
    }
    
    rpar <- model_options_default[["rpar"]]
    if (is.null(rpar) || !is.list(rpar)) {
      stop("'rpar' must be a named list indicating the distributions")
    } else {
      rpar <- model_options_default[["rpar"]] <- lapply(rpar, tolower)
    }
    
    if (!all(unlist(rpar) %in% c("normal",
                                 "triangular",
                                 "uniform"))) {
      stop("Unknown distribution type specified")
    }
  }
  
  # Return the validated list of model options
  cli::cli_alert_success("Model options")
  
  return(model_options_default)
  
}
