#' Function to check the data
#'
#' The function will check the data to see if it is balanced. It will take the
#' necessary steps to balance the data. If it is unable to do so, the code will
#' return suggestions for how to set up the data. 
#' 
#' @return A list of conditions
#' 
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options

prepare_data <- function(db, estim_opt, model_opt) {
  cat(black$bold("Preparing data ...\n"))
  
  N <- length(unique(db[[model_opt[["id"]]]]))
  S <- length(unique(db[[model_opt[["ct"]]]]))

  # Check choice tasks
  if ((N * S) != nrow(db)) {
    cat(yellow$bold("Warning: " %+% reset$silver("Unequal number of choice occasions across individuals. Padding data with NA ... \n")))
    db <- pad_data(db, model_opt)
    cat(green$bold("Success: " %+% reset$silver("Data padded successfully.\n")))
  }
  
  # Split the data if estimating in parallel and return as a matrix
  if (estim_opt$cores > 1) {
    db <- split_data(db, estim_opt, model_opt)
    db <- lapply(db, function(x) as.matrix(x))
  } else {
    db <- as.matrix(db)
  }
  
  # Return the manipulated and checked data
  cat(green$bold("Success: " %+% reset$silver("Data preparation complete.\n")))
  return(db)
}
