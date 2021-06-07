#' Split the data into a list
#'
#' The function takes the data and splits it into roughly equal chunks to be
#' sent to the workers for parallel processing. The function takes care to not
#' split the same individual across multiple workers. 
#' 
#' Note that the function will only split the data correctly if all respondents
#' have the same number of rows. Therefore, it is important that this function
#' is called after the data has been padded with NAs in case of unequal 
#' number of choice tasks.  
#' 
#' @inheritParams prepare_estimation_environment
#' 
#' @return A list of data.frames with length equal to the number of cores to 
#' use in estimation
split_data <- function(db, str_id, cores) {
  # Get the ids and split them across cores
  ids <- db[[str_id]]
  id_index <- split(ids, sort(ids %% cores))
  
  # Split the data according to the split id variable
  db <- lapply(id_index, function(x) {
    db[ids %in% x, ]
  })
  
  # Return the list of data frames
  return(db)
}

#' Get the split index 
#' 
#' The function returns a split index based on the split data.frame passed to
#' the workers.
#' 
#' @inheritParams prepare_estimation_environment
#' 
#' @return A list of row-indexes that can be used to split matrixes to different
#' cores 
get_split_index <- function(db, cores) {
  the_rows <- lapply(db, function(x) {
    nrow(x)
  })
  
  idx <- vector(mode = "list", length = length(db))
  idx[[1]] <- seq_len(the_rows[[1]])
  for (i in 2:length(db)) {
    idx[[i]] <- max(idx[[i - 1]]) + seq_len(the_rows[[i]])
  }
  
  # Return the index
  return(idx)
}
