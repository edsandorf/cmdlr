#' Set default estimation options
#'
#' The function sets default save options for options that are not 
#' specified by the user
#'
#' @param save_opt_input A list of user specified estimation options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

set_save_opt_defaults <- function(save_opt_input) {
  save_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  save_opt[names(save_opt_input)] <- save_opt_input
  
  return(save_opt)
}
