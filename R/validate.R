#' Validates inputs
#' 
#' The function is a wrapper for several cmdlR functions that validates the
#' inputs. The purpose is to ensure correct specification of the model and that
#' all information is available. 
#'
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param summary_opt List of options for summary statistics
#' @param log_lik Log likelihood function
#' 
#' @return A list with updated and validated \code{estim_opt}, \code{model_opt},
#' and \code{save_opt}. 
#' 
#' @export

validate <- function(estim_opt, model_opt, save_opt, summary_opt, log_lik) {
  cat(black("Validating inputs ...\n"))
  
  # Validate estimation options ----
  estim_opt <- validate_estim_opt(estim_opt)
  
  # Validate model options ----
  model_opt <- validate_model_opt(model_opt)
  message(green$bold(symbol$tick) %+% reset$silver("  model_opt() validated.\n"))
  
  # Validate save optioins ----
  save_opt <- validate_save_opt(save_opt)
  if (save_opt$path == file.path(getwd(), "model-01")) {
    cat(blue$bold("Note: " %+% reset$silver(paste0("Outputs are stored in default location: \"",
                                                   file.path(getwd(), "model-01"), "\"\n"))))
  }
  message(green$bold(symbol$tick) %+% reset$silver("  save_opt() validated.\n"))
  
  summary_opt <- validate_summary_opt(summary_opt)
  message(green$bold(symbol$tick) %+% reset$silver("  summary_opt() validated.\n"))
  
  # Validate the log likelihood function ----

  

  
  # Return the list of inputs ----
  return(list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt,
    summary_opt = summary_opt
  ))
}
