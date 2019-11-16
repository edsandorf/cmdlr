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
  estim_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  estim_opt[names(estim_opt_input)] <- estim_opt_input
  
  return(estim_opt)
}
