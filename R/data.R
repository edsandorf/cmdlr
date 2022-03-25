#' Subset the data to only include used variables
#' 
#' The function deparses the supplied function to find only the variables
#' used. 
#' 
#' @param func A user supplied function
#' @param db A tibble or data.frame
#' @param ... Other variables in `db` to keep that are not used in the 
#' supplied function. Must be supplied as character strings.
#' 
#' @examples 
#' ll <- function(param) {
#'   V <- list(
#'     alt1 = asc_1 + b_1 * variable_1,
#'     alt2 = asc_2 + b_2 * var_2
#'   )
#' }
#' 
#' db <- tibble::tibble(
#'   variable_1 = runif(10),
#'   var_2 = runif(10),
#'   id = seq(1, 10, 1),
#'   ct = rep(1:2, 5),
#'   income = sample(100:1000, 10)
#' )
#' 
#' subset_data(ll, db, c("id", "ct"))
#' subset_data(ll, db, "id", "ct")
#' subset_data(ll, db, c("id", "ct"), "income")
#' 
#' @export
subset_data <- function(func,
                        db,
                        ...) {
  
  # Collect additional variables
  other_vars <- list(...)
  
  # Check if all supplied are character vectors
  stopifnot("All additional variables passed to '...' must be characters"= 
              all(do.call(c, lapply(other_vars, is.character))))
  
  # Collect to a vector of additional variables
  other_vars <- do.call(c, other_vars)
  
  # Check which variables are used in the log-likelihood function
  vars_to_keep <- is_used(names(db), func)
  
  # Set the additional variables to keep to TRUE
  vars_to_keep[other_vars] <- TRUE
  
  return(
    db[, vars_to_keep]
  )
}

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
