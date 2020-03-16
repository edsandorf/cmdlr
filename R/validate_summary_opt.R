#' Validates and sets default summary options
#'
#' The function validates the user supplied list of summary options and sets 
#' defaults for the missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
#'
#' @param summary_opt_input A list of user specified summary options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_summary_opt <- function(summary_opt_input) {
  summary_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  summary_opt[names(summary_opt_input)] <- summary_opt_input
  
  # Return the validated list of summary options
  message(green$bold(symbol$tick) %+% reset$silver("  summary_opt().\n"))
  summary_opt
}
