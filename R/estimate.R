#' Estimates the model 
#' 
#' The function is a wrapper for several cmdlR functions that sets up parallel
#' processes and estimates the data. 
#'
#' @param log_lik A log likeliohood function to be maximized/minimized
#' @param estim_opt List of estimation options
#' @param workers A worker cluster for parallel estimation
#' 
#' @export

estimate <- function(log_lik, estim_opt, workers = NULL) {
  # Attach the variables to enable calling by names
  attach()
  on.exit(detach(), add = TRUE)
  
  if (estim_opt$cores > 1) on.exit(parallel::stopCluster(workers), add = TRUE)
}
