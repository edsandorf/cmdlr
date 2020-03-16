#' Validates and sets default estimation options
#'
#' The function validates the user supplied list of estimation options and sets 
#' defaults for missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
#'
#' @param estim_opt_input A list of user specified estimation options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_estim_opt <- function(estim_opt_input) {
  # Set the missing to default values
  estim_opt <- list(
    optimizer = "maxlik",
    method = "bfgs",
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
  
  # Check the optimizer and optimization routine combinations
  msg <- yellow$bold("Warning: " %+% 
                       reset$silver(paste0("\'", tolower(estim_opt[["method"]]), "\'",
                                           " is not an option in ",
                                           "\'", tolower(estim_opt[["optimizer"]]), "\'",
                                           ". Using \'bfgs\' as the optmization routine.\n")))
  
  if (tolower(estim_opt[["optimizer"]]) == "maxlik") {
    estim_opt[["optimizer"]] <- "maxlik"
    if (tolower(estim_opt[["method"]]) %in% c("slsqp", "sbplx")) {
      cat(msg)
      estim_opt[["method"]] <- "BFGS"
    }
    
    if (tolower(estim_opt[["method"]]) == "bfgs") estim_opt[["method"]] <- "BFGS"
    if (tolower(estim_opt[["method"]]) == "bhhh") estim_opt[["method"]] <- "BHHH"
    if (tolower(estim_opt[["method"]]) == "nr") estim_opt[["method"]] <- "NR"
  }
  
  if (tolower(estim_opt[["optimizer"]]) == "nloptr") {
    estim_opt[["optimizer"]] <- "nloptr"
    if (tolower(estim_opt[["method"]]) %in% c("bhhh", "nr")) {
      cat(msg)
      estim_opt[["method"]] <- "NLOPT_LD_LBFGS"
    }
    
    if (tolower(estim_opt[["method"]]) == "bfgs") estim_opt[["method"]] <- "NLOPT_LD_LBFGS"
    if (tolower(estim_opt[["method"]]) == "slsqp") estim_opt[["method"]] <- "NLOPT_LD_SLSQP"
    if (tolower(estim_opt[["method"]]) == "sbplx") estim_opt[["method"]] <- "NLOPT_LN_SBPLX"
  }
  
  # Return the validated list of estimation options
  estim_opt
}
