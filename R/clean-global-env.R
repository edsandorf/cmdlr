#' Tidying the environment to prepare for estimation
#' 
#' A small function to tidy and prepare the work space and environment to run
#' the \code{cmdlR} functions. It is highly recommended to call the function
#' directly using the \code{::} notation to avoid having to load packges twice.
#' 
#' @export

clean_global_env <- function() {
  # Delete all non-essential objects from the work space
  rm(list = ls(all.names = TRUE), envir = .GlobalEnv)
  
  # Detach all non-essential objects from all environments
  detach_objects(all = TRUE)
  
  # Turn off file-writing if connection is open
  if(sink.number() > 0) sink()
  
  # Run the garbage collector
  invisible(gc(verbose = FALSE))
}
