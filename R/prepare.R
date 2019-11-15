#' A function that prepares the data for estimation
#'
#' The function is a wrapper for several cmdlR functions aimed at preparing the
#' data for estimation. 
#'
#' @param db Data
#' @param opts List of \code{estim_opt}, \code{model_opt}, \code{save_opt}.
#' @return A list with \code{estim_opt}, \code{model_opt}, \code{save_opt},
#' \code{db} and \code{workers}. 
#' 
#' @export

prepare <- function(opts, db) {
  cat(black("Preparing data for estimation ... \n"))
  
  estim_opt <- opts[["estim_opt"]]
  model_opt <- opts[["model_opt"]]
  save_opt <- opts[["save_opt"]]
  
  #-----------------------------------------------------------------------------
  # Check the data
  #-----------------------------------------------------------------------------
  db <- prepare_data(db, estim_opt, model_opt)
  
  #-----------------------------------------------------------------------------
  # Starting values
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  # Parallel estimation
  #-----------------------------------------------------------------------------
  if (estim_opt$cores > 1) {
    cat(black$bold("Preparing workers for parallel estimation. \n"))
    
    # Create the cluster of workers
    workers <- parallel::makeCluster(estim_opt$cores, type = "PSOCK")
    
    prepare_parallel(db, estim_opt, model_opt, save_opt, workers)
    
    # Print exit message if successful
    cat(green$bold("Success: " %+% reset$silver("Workers prepared.\n")))
  }
  
  #-----------------------------------------------------------------------------
  # Return the list of inputs, data and workers
  #-----------------------------------------------------------------------------
  invisible(gc(verbose = FALSE))
  
  cat(green$bold("Success: " %+% reset$silver("Data prepared and ready for estimation! \n")))
  
  return(list(
    estim_opt = estim_opt,
    model_opt = model_opt,
    save_opt = save_opt,
    db = db, 
    workers = workers
  ))
}
