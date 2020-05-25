#' Function to check the data
#'
#' The function will check the data to see if it is balanced. It will take the
#' necessary steps to balance the data. If it is unable to do so, the code will
#' return suggestions for how to set up the data. 
#' 
#' This function is intended for internal use only.
#' 
#' @inheritParams estimate
#' 
#' @return A list of conditions

prepare_data <- function(db, estim_opt, model_opt) {
  
  N <- length(unique(db[[model_opt[["id"]]]]))
  S <- length(unique(db[[model_opt[["ct"]]]]))

  # Check choice tasks
  if ((N * S) != nrow(db)) {
    message(yellow$bold(symbol$warning), "  Unequal number of choice occasions across individuals. Padding the data with NA.")
    db <- pad_data(db, model_opt)
  }
  
  # Split the data if estimating in parallel and return as a matrix
  if (estim_opt$cores > 1) {
    db <- split_data(db, estim_opt, model_opt)
  } 
  
  # Return the manipulated and checked data
  message(green$bold(symbol$tick), "  Data")
  db
}
