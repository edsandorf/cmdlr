#' Function to split the data
#'
#' The function takes the data and splits it into roughly equal chunks to be
#' sent to the workers for parallel processing. The function takes care to not
#' split the same individual across multiple workers. 
#' 
#' It is important that the function is run after \code{check_data}. The code
#' only splits individuals and observations correctly if there are equal
#' number of rows per individual. 
#' 
#' @param db A data frame
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#'
#' @return A list of data matrices with length equal to \code{cores}.

split_data <- function(db, estim_opt, model_opt) {
  # Check if the data is balanced
  N <- length(unique(db[[model_opt[["id"]]]]))
  S <- length(unique(db[[model_opt[["ct"]]]]))
  
  if ((N * S) != nrow(db)) {
    stop("Unequal number of rows per individual. Run check_data(). \n")
  }
  
  # Get the ids and split them across cores
  ids <- deframe(db[, model_opt$id])
  id_index <- split(ids, sort(ids %% estim_opt$cores))
  
  # Split the data according to the split id variable
  db <- lapply(id_index, function(x) {
    db[ids %in% x, ]
  })
  
  # Return the list of data frames
  return(db)
}
