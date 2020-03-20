#' Function to prepare the workers for parallel estimation
#' 
#' The function will set up the workers, split the data and export necessary
#' objects and functions to the workers.
#' 
#' @param db Data
#' @param draws A list of random draws used in simulation
#' @param inputs A list containing options and data to be passed to the worker
#' @param workers A cluster of workers
#' 
#' @return A cluster of workers

prepare_workers <- function(db, draws, inputs, workers) {
  
  # Export packages to the worker
  pkgs <- c("maxLik", "numDeriv")
  parallel::clusterCall(workers, function(pkgs) {
    lapply(pkgs, require, character.only = TRUE)
    NULL
  }, pkgs)
  
  # Export functions and indices to the workers
  parallel::clusterExport(workers,
                          c("lik", "attach_objects", "detach_objects", "inputs"),
                          envir = environment())
  
  # Export data to the workers 
  parallel::parLapply(workers, db, function(x) {
    assign("db", x, envir = globalenv())
    NULL
  })
  
  # Export the draws to the workers
  if (isTRUE(inputs$model_opt$mixing)) {
    parallel::parLapply(workers, draws, function(x) {
      assign("draws", x, envir = globalenv())
      NULL
    })
  }
  
  # Save information about what is loaded on the workers
  if (inputs$save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    file_path <- file.path(getwd(), inputs$save_opt$path, paste0(make_model_name(inputs$model_opt$name), "-worker-info.txt"))
    sink(file_path)
    summary_worker_info(worker_info)
    sink()
    cat(blue$bold(symbol$info), paste0("  Worker information saved to \"",
                                                   file_path, "\"\n"))
  }
  
  # Return the workers
  cat(green$bold(symbol$tick), "  Workers\n")
  workers
}
