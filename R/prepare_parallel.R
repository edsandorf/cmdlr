#' Function to prepare and set up for parallel estimation
#' 
#' The function will set up the workers, split the data and export necessary
#' objects and functions to the workers.
#' 
#' @param estim_opt List of estimation options
#' @param save_opt List of options for saving outputs
#' 
#' @export

prepare_parallel <- function(estim_opt, save_opt) {
  cat(black$bold("Preparing workers ...\n"))
  
  # Prepare the data
  
  # Create the cluster of workers
  workers <- parallel::makeCluster(estim_opt$cores, type = "PSOCK")
  
  # Save information about what is loaded on the workers
  if (save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    summary_worker_info(worker_info)
    save_path <- paste0(save_opt$path, "-worker-info.txt")
    cat(silver("" %+% black$bold("Worker information written to: ") %+% save_path))
  }
  
  # Print exit message if successful
  cat(green$bold("Success: " %+% reset$silver("Workers prepared. Estimation starting ...\n")))
}
