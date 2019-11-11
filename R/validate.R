#' Validates the inputs to the model estimation
#' 
#' The function is a wrapper for several cmdlR functions that checks the inputs,
#' controls and data. The purpose is to make sure that the combination of 
#' inputs are allowed and supported by the package. 
#'
#' @param estim_opt List of estimation options
#' 
#' @export

validate <- function(estim_opt) {
  
  # Check that the number of cores are reasonable
  if (estim_opt$cores > 1) {
    cat(black("Checking parallel options ...\n"))
    if (estim_opt$cores < parallel::detectCores()) {
      cat(green$bold("Success: " %+% reset$silver("Number of cores OK.\n")))
    } else {
      estim_opt$cores <<- max(1L, parallel::detectCores() - 1L)
      cat(yellow$bold("Warning: " %+% reset$silver("Cores exceed available. Set to max - 1.\n")))
    }
  }
}
