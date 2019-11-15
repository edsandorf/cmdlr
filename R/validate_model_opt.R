#' Set default model options
#'
#' The function sets default model options for options that are not 
#' specified by the user
#'
#' @param model_opt_input A list of user specified estimation options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_model_opt <- function(model_opt_input) {
  model_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  model_opt[names(model_opt_input)] <- model_opt_input
  
  return(model_opt)
}
