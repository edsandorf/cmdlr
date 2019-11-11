#' Function for getting worker info
#'
#' The function will loop through the workers to get information about what
#' objects are loaded on each worker and how the work load is distributed.
#' 
#' @param workers A cluster of workers
#'
#' @export

get_worker_info <- function(workers) {
  parallel::parLapply(workers, seq_along(workers), function(x) {
    
    # Get the object, size and class
    worker_info <- lapply(ls(.GlobalEnv), function(x) {
      cbind(x,
            pryr::object_size(x, envir = .GlobalEnv),
            class(get(x, envir = .GlobalEnv)))
    })
    info <- Reduce(rbind, info)
    colnames(info) <- c("Object", "Size (bytes)", "Class")
    
    # Get the packages loaded on the workers
    worker_pkgs <- search()
    
    # Return the information as a list
    return(list(
      worker_info,
      worker_pkgs
    ))
  })
}

#' Function for summarizing worker information
#' 
#' The function summarizes and prepares to print the worker information to a
#' .txt file
#' 
#' @param worker_info A worker information object from get_worker_info()
#'
#' @export
summary_worker_info <- function(worker_info) {
  
}
