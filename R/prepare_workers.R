#' Function preparing the workers
#' 
#' When we estimate the model using multiple cores, we need to set up the workers
#' such that the correctly split data, draws, packages and settings are available
#' to the log-likelihood function. 
#' 
#' The function is meant for internal use only. 
#' 
#' @inheritParams estimate
#' @param draws A list of draws where each list element correspond to one dimension
#' or if we are using multiple cores, a list of lists where the top-level list
#' corresponds to the cores and the lower-level list corresponds to the draws.
#' @param workers A PSOCK or Fork cluster of workers created using 
#' \code{parallel::makeCluster()} 
#' 
#' @return The function does not return any objects. 

prepare_workers <- function(db, draws, workers, model_opt, save_opt) {
  # Export packages to the worker
  pkgs <- c("maxLik", "numDeriv", "rlang")
  parallel::clusterCall(workers, function(pkgs) {
    lapply(pkgs, require, character.only = TRUE)
    NULL
  }, pkgs)
  
  # Export functions and indices to the workers
  parallel::clusterExport(workers,
                          c("ll", "detach_objects",
                            "prepare_estim_env", "model_opt"),
                          envir = environment())
  
  # Export data to the workers 
  parallel::parLapply(workers, db, function(x) {
    assign("db", x, envir = globalenv())
    NULL
  })
  
  # Export the draws to the workers
  if (model_opt$mixing) {
    parallel::parLapply(workers, draws, function(x) {
      assign("draws", x, envir = globalenv())
      NULL
    })
  }
  
  # Prepare the estimation environment
  parallel::parLapply(workers, seq_along(workers), function(x) {
    db <- get("db", envir = .GlobalEnv)
    model_opt <- get("model_opt", envir = .GlobalEnv)
    if (model_opt$mixing) {
      draws <- get("draws", envir = .GlobalEnv)
    } else {
      draws <- NULL
    }
      
    assign("estim_env", prepare_estim_env(db, draws, model_opt), envir = .GlobalEnv)
  })
  
  # Save information about what is loaded on the workers
  if (save_opt$save_worker_info) {
    worker_info <- get_worker_info(workers)
    if (is.null(save_opt$path)) save_opt$path <- ""
    file_path <- file.path(getwd(), save_opt$path, paste0(make_model_name(save_opt$name), "-worker-info.txt"))
    sink(file_path)
    summary_worker_info(worker_info)
    sink()
    message(blue$bold(symbol$info), paste0("  Worker information saved to \"",
                                                   file_path, "\""))
  }
  
  # Return the workers
  message(green$bold(symbol$tick), "  Workers")
}
