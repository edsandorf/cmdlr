#' Estimates the model 
#' 
#' The function is a wrapper for several cmdlR functions that sets up parallel
#' processes and estimates the data. 
#'
#' @param inputs List of inputs to the estimation. The result of running
#' \code{prepare}. 
#' 
#' @export

estimate <- function(inputs) {
  cat(black("Estimation starting ... \n"))
  
  estim_opt <- inputs[["estim_opt"]]
  model_opt <- inputs[["model_opt"]]
  save_opt <- inputs[["save_opt"]]
  db <- inputs[["db"]]
  workers <- inputs[["workers"]]
  
  # Attach the variables to enable calling by names
  attach()
  on.exit(detach(), add = TRUE)
  
  if (estim_opt$cores > 1) on.exit(parallel::stopCluster(workers), add = TRUE)
  
  cat(green$bold("Success: " %+% reset$silver("Estimation complete!\n")))
}
