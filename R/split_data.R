#' Function to split the data
#'
#' The function takes the data and splits it into roughly equal chunks to be
#' sent to the workers for parallel processing. The function takes care to not
#' split the same individual across multiple workers.
#' 
#' @param db A data frame
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#'
#' @return A list of data matrices with length equal to \code{cores}.

split_data <- function(db, estim_opt, model_opt) {
  
}
