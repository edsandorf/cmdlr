#' Validate and set default estimation controls
#'
#' The function validates the user supplied list of estimation controls and sets
#' defaults. The defaults follow those set by the respective optimizer. See
#' \code{\link{maxLik}} and \code{\link{ucminf}} for details. Additional options
#' include `optimizer`, `method`, `cores`, `calculate_hessian` and
#' `calculate_jacobian`. 
#' 
#' The function is called once within \code{\link{estimate}}. 
#' 
#' @param control A list of control options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values
set_controls <- function(control) {
  # A list of default values
  control_default <- list(
    optimizer = "maxlik",
    method = "BFGS",
    cores = 1,
    calculate_hessian = TRUE,
    calculate_jacobian = TRUE,
    tol = 1e-08,
    reltol = sqrt(.Machine$double.eps),
    gradtol = 1e-06,
    steptol = 1e-10,
    lambdatol = 1e-6,
    print_level = 2,
    iterlim = 500,
    grad = "forward",
    gradstep = c(1e-6, 1e-8),
    stepmax = 1
  )
  
  # If no list of controls is supplied, return the defaults
  if (is.null(control)) {
    return(control_default)
  }
  
  # Replace the defaults with control
  control_default[names(control)] <- control
  
  # Fix the method and optimizer and check 
  method <- control_default$method <- toupper(control_default$method)
  optimizer <- control_default$optimizer <- tolower(control_default$optimizer)
    
  # Check that the specified optimizer is correct
  if (!(optimizer %in% c("maxlik", "ucminf", "bgw"))) {
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
  if (control_default$cores > parallel::detectCores()) {
    stop("The number of specified cores exceed available.")
    
  }
  
  return(control_default)
}
