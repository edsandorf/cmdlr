#' Validates and sets default estimation options
#'
#' The function validates the user supplied list of estimation options and sets 
#' defaults for missing options. The checks are aggressive and will stop
#' the estimation and ask the user to respecify incorrect inputs. This is to 
#' ensure that the user intends to use these exact options.
#'
#' @param estim_opt_input A list of user specified estimation options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_estim_opt <- function(estim_opt_input) {
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
    robust_vcov = TRUE
  )
  
  # Replace the non-specified values with default values
  estim_opt[names(estim_opt_input)] <- estim_opt_input
  
  # Check that the optimizer and method is specified correctly ----
  if (is.null(estim_opt[["optimizer"]]) || is.null(estim_opt[["method"]])) {
    message(red$bold(symbol$cross) %+% reset$silver("  estim_opt().\n"))
    stop("You must specify the optimizer and optimization routine to use in estim_opt.")
  }
  
  # Check whether the optimizer can use the specified method
  if (tolower(estim_opt[["optimizer"]]) %in% c("maxlik", "nloptr")) {
    
    # Check maxlik methods
    if (tolower(estim_opt[["optimizer"]]) == "maxlik") {
      if (tolower(estim_opt[["method"]]) %in% c("bfgs", "bhhh", "nr")) {
        # Set the correct call to the optimizer
        if (tolower(estim_opt[["method"]]) == "bfgs") estim_opt[["method"]] <- "BFGS"
        if (tolower(estim_opt[["method"]]) == "bhhh") estim_opt[["method"]] <- "BHHH"
        if (tolower(estim_opt[["method"]]) == "nr") estim_opt[["method"]] <- "NR"
      } else {
        message(red$bold(symbol$cross) %+% reset$silver("  estim_opt().\n"))
        stop("The optimization method must be either 'bfgs', 'bhhh' or 'nr' when using 'maxlik'.")
      }
    }
    
    # Check nloptr options
    if (tolower(estim_opt[["optimizer"]]) == "nloptr") {
      if (tolower(estim_opt[["method"]]) %in% c("bfgs", "slsqp", "sbplx")) {
        if (tolower(estim_opt[["method"]]) == "bfgs") estim_opt[["method"]] <- "NLOPT_LD_LBFGS"
        if (tolower(estim_opt[["method"]]) == "slsqp") estim_opt[["method"]] <- "NLOPT_LD_SLSQP"
        if (tolower(estim_opt[["method"]]) == "sbplx") estim_opt[["method"]] <- "NLOPT_LN_SBPLX"
      } else {
        message(red$bold(symbol$cross) %+% reset$silver("  estim_opt().\n"))
        stop("The optimization method must be either 'bfgs', 'slsqp' or 'sbplx' when using 'nloptr'.")
      }
    }
  } else {
    message(red$bold(symbol$cross) %+% reset$silver("  estim_opt().\n"))
    stop("The optimizer is either 'maxlik' or 'nloptr'. See package documentation for more details.")
  }

  # Check that enough cores are available ----
  if (estim_opt$cores > parallel::detectCores()) {
    message(red$bold(symbol$cross) %+% reset$silver("  estim_opt().\n"))
    stop(paste0("The number of specified cores exceed the number of cores available. You have ", parallel::detectCores(), " cores available (including hyperthreading). It is advised to use less than this in estimation.\n"))
  }
  
  # Return the validated list of estimation options
  message(green$bold(symbol$tick) %+% reset$silver("  estim_opt().\n"))
  estim_opt
}
