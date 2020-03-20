#' Validates inputs
#' 
#' The function is a wrapper for several cmdlR functions that validates the
#' inputs. The purpose is to ensure correct specification of the model and that
#' all information is available. 
#'
#' @param lik Likelihood function
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param summary_opt List of options for summary statistics
#' 
#' @return A list with updated and validated \code{estim_opt}, \code{model_opt},
#' and \code{save_opt}. 
#' 
#' @export

validate <- function(lik, estim_opt, model_opt, save_opt, summary_opt) {
  cat("Validating inputs\n")
  
  # Validate estimation options ----
  estim_opt <- validate_estim_opt(estim_opt)
  assign("estim_opt", estim_opt, envir = .GlobalEnv)
  
  # Validate model options ----
  model_opt <- validate_model_opt(model_opt)
  assign("model_opt", model_opt, envir = .GlobalEnv)
  
  # Validate save options ----
  save_opt <- validate_save_opt(save_opt)
  assign("save_opt", save_opt, envir = .GlobalEnv)
  
  # Validate summary options ----
  summary_opt <- validate_summary_opt(summary_opt)
  assign("summary_opt", summary_opt, envir = .GlobalEnv)
  
  # Validate the log likelihood function ----

}
