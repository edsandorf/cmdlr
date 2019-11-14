#' Validates the inputs to the model estimation
#' 
#' The function is a wrapper for several cmdlR functions that checks the inputs,
#' controls and data. The purpose is to make sure that the combination of 
#' inputs are allowed and supported by the package. 
#'
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param log_lik Log likelihood function
#' 
#' @export

validate <- function(estim_opt, model_opt, save_opt, db, log_lik) {
  #-----------------------------------------------------------------------------
  # Set the default options
  #-----------------------------------------------------------------------------
  # Set default estimation options
  estim_opt <- set_estim_opt_defaults(estim_opt)
  cat(green$bold("Success: " %+% reset$silver("Default options set for estim_opt().")))

  # Set default model options
  model_opt <- set_model_opt_defaults(model_opt)
  cat(green$bold("Success: " %+% reset$silver("Default options set for model_opt().")))
  
  # Set default saving options
  save_opt <- set_save_opt_defaults(save_opt)
  if (save_opt$path == file.path(getwd(), "model-01")) {
    cat(blue$bold("Note: " %+% reset$silver(paste0("Outputs are stored in default location: \"",
                                                   file.path(getwd(), "model-01"), "\"\n"))))
  }
  cat(green$bold("Success: " %+% reset$silver("Default options set for save_opt().")))
  
  #-----------------------------------------------------------------------------
  # Check the data
  #-----------------------------------------------------------------------------
  db <- check_data(db, estim_opt, model_opt)
  
  #-----------------------------------------------------------------------------
  # Check parallel options
  #-----------------------------------------------------------------------------
  if (estim_opt$cores > 1) {
    cat(black("Checking parallel options ...\n"))
    
    # Check the number of cores
    if (estim_opt$cores < parallel::detectCores()) {
      cat(green$bold("Success: " %+% reset$silver("Number of cores OK.\n")))
    } else {
      estim_opt$cores <- max(1L, parallel::detectCores() - 1L)
      cat(yellow$bold("Warning: " %+% reset$silver("Cores exceed available. Set to max - 1.\n")))
    }
    
    # Split the data
    db <- split_data(db, estim_opt, model_opt)
    
  }
  
  #-----------------------------------------------------------------------------
  # Return the list of inputs
  #-----------------------------------------------------------------------------
  return(list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt,
    db = db,
    log_lik
  ))
}
