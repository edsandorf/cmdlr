#' A function that prepares the data for estimation
#'
#' The function is a wrapper for several cmdlR functions aimed at preparing the
#' data for estimation. 
#'
#' @param db Data¨
#' @param log_lik Log likelihood function
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param summary_opt List of options for summary statistics
#' 
#' @return A list with \code{estim_opt}, \code{model_opt}, \code{save_opt},
#' \code{db} and \code{workers}. 
#' 
#' @export

prepare <- function(db, log_lik, estim_opt, model_opt, save_opt, summary_opt) {
  cat("Preparing for estimation \n")
  
  # Prepare the data ----
  db <- prepare_data(db, estim_opt, model_opt)

  # Prepare draws ----
  # draws <- prepare_draws()
  
  # Parallel estimation ----
  if (estim_opt$cores > 1) {
    workers <- prepare_workers(db, estim_opt, model_opt, save_opt)
  } else {
    workers <- NULL
  }
  
  # Prepare the log likelihood function ----
  ll_func <- prepare_log_lik(log_lik, estim_opt, workers)
  
  # Prepare the numerical gradient ----
  num_grad <- prepare_num_grad()
  
  # Starting values ----
  # prepare_starting_values()
  
  # Create the list of inputs ----
  inputs <- list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt,
    summary_opt = summary_opt,
    db = db, 
    workers = workers,
    log_lik = ll_func,
    num_grad = num_grad
  )
  
  # Clean up global environment
  rm(estim_opt, model_opt, save_opt, summary_opt, envir = .GlobalEnv)
  
  # Return the inputs
  inputs
}
