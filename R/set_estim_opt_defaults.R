#' Check estimation options
#'
#' The function checks the list of estimation options specified by the user. If
#' no list of options is supplied or some of the options are not defined, the 
#' function will supply reasonable default values. The list of estimation options
#' specifies options related to the optimization routines used.
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
