#' A function that prepares the data for estimation
#'
#' The function is a wrapper for several cmdlR functions aimed at preparing the
#' data for estimation. 
#'
#' @param db Data
#' @param opts List of \code{estim_opt}, \code{model_opt}, \code{save_opt}.
#' @param log_lik Log likelihood function
#' 
#' @return A list with \code{estim_opt}, \code{model_opt}, \code{save_opt},
#' \code{db} and \code{workers}. 
#' 
#' @export

prepare <- function(opts, db, log_lik) {
  cat("Preparing for estimation \n")
  
  # Extract options ----
  estim_opt <- opts[["estim_opt"]]
  model_opt <- opts[["model_opt"]]
  save_opt <- opts[["save_opt"]]
  summary_opt <- opts[["summary_opt"]]
  
  # Prepare the data ----
  db <- prepare_data(db, estim_opt, model_opt)
  
  # Starting values ----
  # prepare_starting_values()
  
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
  
  # Return the list of inputs, data and workers ----
  return(list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt,
    summary_opt = summary_opt,
    db = db, 
    workers = workers,
    log_lik = ll_func,
    num_grad = num_grad
  ))
}
