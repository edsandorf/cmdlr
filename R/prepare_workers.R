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
  # Extract relevant information from model_opt
  N <- model_opt$N
  S <- model_opt$S
  R <- model_opt$R
  
  # Export packages to the worker ----
  pkgs <- c("maxLik", "numDeriv", "rlang")
  parallel::clusterCall(workers, function(pkgs) {
    lapply(pkgs, require, character.only = TRUE)
    NULL
  }, pkgs)
  
  # Export functions and indices to the workers ----
  parallel::clusterExport(workers, "ll", envir = environment())
  
  # Split the data ----
  db <- split_data(db, estim_opt, model_opt)
  
  # Split the draws ----
  the_rows <- lapply(db, function(x) {
    nrow(x)
  })
  rows_index <- vector(mode = "list", length = length(db))
  rows_index[[1]] <- seq_len(the_rows[[1]])
  for (i in 2:length(db)) {
    rows_index[[i]] <- max(rows_index[[i - 1]]) + seq_len(the_rows[[i]])
  }
  
  draws <- lapply(rows_index, function(i) {
    lapply(draws, function(x) {
      x[i, , drop = FALSE]
    })
  })
  
  # Split alt_avail ----
  alt_avail <- lapply(rows_index, function(i) {
    lapply(model_opt$alt_avail, function(x) {
      x[i]
    })
  })
  
  # Set up the estimation environment ----
  estim_opt_list <- lapply(seq_along(db), function(i) {
    # Create the estimation environment
    estim_env <- rlang::env()
    
    # Define indexes
    index_list <- list(
      N = length(unique(db[[i]][[model_opt[["id"]]]])),
      S = length(unique(db[[i]][[model_opt[["ct"]]]])),
      J = length(model_opt$alt_avail),
      alt_avail = alt_avail[[i]],
      choice_var = db[[i]][[model_opt$choice]]
    )
    list2env(index_list, envir = estim_env)
    
    # Add the data to the estimation environment
    list2env(as.list(db[[i]]), envir = estim_env)
    
    # Add the draws to the estimation environment
    if (model_opt$mixing) {
      list2env(as.list(draws[[i]]), envir = estim_env)
    }
    
    estim_env
  })
  
  parallel::parLapply(workers, estim_opt_list, function(x) {
    estim_env <<- x
    NULL
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
