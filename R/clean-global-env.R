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

#' Detach objects from the environment
#' 
#' This function detaches the objects passed through the list \code{objects}. 
#' Each list element is an object to be detached. 
#' 
#' @param objects An un-named list of objects to be attached
#' @param all If \code{TRUE} the function detaches all non-essential objects and
#' packages from the global environment.
#' 
#' @return Nothing
#' 
#' @export
detach_objects <- function (objects = NULL, all = FALSE) {
  # Check if the list of objects to be detached is non-empty
  if (!is.null(objects)) {
    lapply(objects, function(x) {
      if (is.data.frame(x)) detach(x)
      if (is.vector(x)) detach(as.list(x))
    })
  }
  
  # Check if we are detaching all objects
  if (all) {
    subset_environment <- substring(search(), 1L, 8L) != "package:" &
      search() != ".GlobalEnv" & search() != "Autoloads" & 
      search() != "CheckExEnv" & search() != "tools:rstudio" &
      search() != "TempEnv"
    
    object_positions <- (1L:length(search()))[subset_environment]
    
    for (i in 1L:length(object_positions)) {
      if (length(object_positions) > 0L){
        detach(pos = object_positions[1L])
        object_positions <- (1L:length(search()))[subset_environment]
      }
    }
  }
}
