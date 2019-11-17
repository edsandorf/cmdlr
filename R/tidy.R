#' Tidying the environment to prepare for estimation
#' 
#' A small function to tidy and prepare the work space and environment to run
#' the \code{cmdlR} functions. It is highly recommended to call the function
#' directly using the \code{::} notation to avoid having to load packges twice.
#' 
#' @param clean If \code{TRUE} deletes all objects from the work space
#' 
#' @export

tidy <- function(clean = FALSE) {
  # Delete all non-essential objects from the work space
  if (clean) {
    rm(list = ls(all.names = TRUE))
  }
  
  # Detach all non-essential objects from all environments
  detach_all()
  
  # Run the garbage collector
  invisible(gc(verbose = FALSE))
}
