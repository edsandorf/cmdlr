#' Function to prepare and set up for parallel estimation
#' 
#' The function will set up the workers, split the data and export necessary
#' objects and functions to the workers.
#' 
#' @param estim_opt List of estimation options
#' 
#' @export

prepare_parallel <- function(estim_opt) {
  cat(crayon::black$bold("Preparing workers ...\n"))
  
  # Prepare the data
  
  
  # Save information about what is loaded on the workers
  if (save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    summary_worker_info(worker_info)
    save_path <- paste0(save_opt$path, "-worker-info.txt")
    cat(crayon::silver("" %+% crayon::black$bold("Worker information written to: ") %+% save_path))
  }
  
  # Print exit message if successful
  cat(crayon::green$bold("Success preparing workers. Estimation starting ...\n"))
}
