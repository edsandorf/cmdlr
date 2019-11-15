#' Validates the inputs
#' 
#' The function is a wrapper for several cmdlR functions that validates the
#' inputs. The purpose is to ensure correct specification of the model and that
#' all information is available. 
#'
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param log_lik Log likelihood function
#' 
#' @return A list with updated and validated \code{estim_opt}, \code{model_opt},
#' and \code{save_opt}. 
#' 
#' @export

validate <- function(estim_opt, model_opt, save_opt, log_lik) {
  cat(black("Validating inputs ...\n"))
  
  #-----------------------------------------------------------------------------
  # Set the default options
  #-----------------------------------------------------------------------------
  # Set default estimation options
  estim_opt <- validate_estim_opt(estim_opt)
  cat(green$bold("Success: " %+% reset$silver("Default options set for estim_opt().")))

  # Set default model options
  model_opt <- validate_model_opt(model_opt)
  cat(green$bold("Success: " %+% reset$silver("Default options set for model_opt().")))
  
  # Set default saving options
  save_opt <- validate_save_opt(save_opt)
  if (save_opt$path == file.path(getwd(), "model-01")) {
    cat(blue$bold("Note: " %+% reset$silver(paste0("Outputs are stored in default location: \"",
                                                   file.path(getwd(), "model-01"), "\"\n"))))
  }
  cat(green$bold("Success: " %+% reset$silver("Default options set for save_opt().")))
  
  #-----------------------------------------------------------------------------
  # Check parallel options and set up workers
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
  }
  
  
  
  #-----------------------------------------------------------------------------
  # Return the list of inputs
  #-----------------------------------------------------------------------------
  cat(green$bold("Success: " %+% reset$silver("All options validated! \n")))
  
  return(list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt
  ))
}
