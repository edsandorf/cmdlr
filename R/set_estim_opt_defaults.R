#' Set default estimation options
#'
#' The function sets default estimation options for options that are not 
#' specified by the user
#'
#' @param estim_opt_input A list of user specified estimation options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

set_estim_opt_defaults <- function(estim_opt_input) {
  estim_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  estim_opt[names(estim_opt_input)] <- estim_opt_input
  
  return(estim_opt)
}
