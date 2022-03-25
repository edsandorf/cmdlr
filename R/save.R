#' Stores the results of the model to files
#'
#' This function takes the model object and the list \code{save_opt} and stores
#' the specified outputs to files. The model object is stored to an .rds file, 
#' model summary created using \code{\link{summarize}} is stored to a .txt file
#' using \code{sink()}, and hessian and variance-covariance matrices are stored
#' as .csv files. Appropriate checks should be in place to avoid over-writing
#' existing files and to close open sink connections. However, it is on the user
#' to be careful with file names to avoid any chance of loosing data.  
#'
#' @param x A 'cmdlr' model object
#' @param path The file path to the folder where the output should be saved. 
#' The default is the current working directory. The file name is inferred 
#' based on the model name and the 'what' argument.
#' @param what What output to save. One of: 'object', 'summary', 'hessian', 
#' 'vcov'. The default is the model object.
#' @param ... Additional parameters passed to the function. For example, 
#' 'robust' if you are saving the variance-covariance matrix
#' 
#' @export
save <- function(x, path = getwd(), what = "object", ...) {
  
  # For storage purposes change the name of the model object to lower case and 
  # '-' separated. 
  path_name <- make_model_name(get_name(x))
  
  
  switch(what,
         object = save_object(x, path),
         summary = save_summary(x, path),
         hessian = save_hessian(x, path),
         vcov = save_vcov(x, path, ...))
  
}

#' Save the model object
#'
#' @inheritParams save
save_object <- function(x, path, ...) {
  
}

#' Save the summary to file
#'
#' @inheritParams save
save_summary <- function(x, path, ...) {
  
}

#' Save the hessian matrix
#'
#' @inheritParams save
save_hessian <- function(x, path, ...) {
  
}

#' Save the variance covariance matrix
#'
#' @inheritParams save
save_vcov <- function(x, path, ...) {
  
}

#' Create the model name used when saving the data
#'
#' @param x A string
make_model_name <- function(x) {
  return(
    paste(Sys.Date(), 
          gsub("([.\\s+])+", "-", tolower(x), perl = TRUE),
          sep = "-")
  )
}
