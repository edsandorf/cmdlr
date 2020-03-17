#' Function to prepare the workers for parallel estimation
#' 
#' The function will set up the workers, split the data and export necessary
#' objects and functions to the workers.
#' 
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' @param save_opt List of options for saving outputs
#' 
#' @return A cluster of workers

prepare_workers <- function(db, estim_opt, model_opt, save_opt) {
  # Create the cluster of workers
  workers <- parallel::makeCluster(estim_opt$cores, type = "PSOCK")
  
  # Export packages to the worker
  pkgs <- c("maxLik", "numDeriv")
  parallel::clusterCall(workers, function(pkgs) {
    lapply(pkgs, require, character.only = TRUE)
    NULL
  }, pkgs)
  
  # Export functions and indices to the workers
  parallel::clusterExport(workers, c("log_lik", "attach_objects", "detach_objects",
                                     "model_opt"))
  
  # Export data to the workers 
  parallel::clusterApply(workers, db, function(x) {
    db <<- x
    NULL
  })
  
  # Save information about what is loaded on the workers
  if (save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    summary_worker_info(worker_info)
    save_path <- paste0(save_opt$path, "-worker-info.txt")
    cat(blue$bold(symbol$info), paste0("  Worker information written to \"",
                                                   save_path, "\"\n"))
  }
  
  # Return the workers
  cat(green$bold(symbol$tick), "  Workers\n")
  workers
}
