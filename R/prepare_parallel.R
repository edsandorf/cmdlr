#' Function to prepare and set up for parallel estimation
#' 
#' The function will set up the workers, split the data and export necessary
#' objects and functions to the workers.
#' 
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' @param workers A worker cluster for parallel estimation

prepare_parallel <- function(db, estim_opt, model_opt, save_opt, workers) {

  # Save information about what is loaded on the workers
  if (save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    summary_worker_info(worker_info)
    save_path <- paste0(save_opt$path, "-worker-info.txt")
    cat(blue$bold("Note: " %+% reset$silver(paste0("Worker information written to \"",
                                                   save_path, "\"\n"))))
  }
}
