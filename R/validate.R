#' Validates the inputs to the model estimation
#' 
#' The function is a wrapper for several cmdlR functions that checks the inputs,
#' controls and data. The purpose is to make sure that the combination of 
#' inputs are allowed and supported by the package. 
#'
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' 
#' @export

validate <- function(estim_opt, model_opt, save_opt) {
  # Set estimation options
  estim_opt <- tryCatch({
    set_estim_opt_defaults(estim_opt)
  }, warning = function(w) {
    cat(yellow$bold("Warning: "))
  }, error = function(e) {
    cat(red$bold("Error: " %+% reset$silver("Failed to set default options for estim_opt. Check your inputs.")))
  })
  cat(green$bold("Success: " %+% reset$silver("Default estimation options set for unspecified options.")))
  
  
  
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
