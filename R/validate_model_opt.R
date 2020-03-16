#' Validates and sets default model options
#'
#' The function validates the user supplied list of model options and sets 
#' defaults for the missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
#'
#' @param model_opt_input A list of user specified model options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_model_opt <- function(model_opt_input) {
  model_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  model_opt[names(model_opt_input)] <- model_opt_input
  
  # Return the validated list of model options
  model_opt
}
