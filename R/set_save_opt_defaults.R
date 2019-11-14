#' Set default estimation options
#'
#' The function sets default save options for options that are not 
#' specified by the user
#'
#' \describe{
#'   \item{path}{The path and file name for your output. Defaults to current
#'   working directory and 'model-01'}
#'   \item{save_summary}{If \code{TRUE} save summary to .txt file specified
#'   in \code{path}. Default is \code{FALSE}.}
#'   \item{save_model_object}{If \code{TRUE} save model object to .rds file
#'   specified in \code{path}. Default is \code{FALSE}.}
#'   \item{save_hessian}{If \code{TRUE} save hessian to .csv file specified in
#'   \code{path}. Default is \code{FALSE}.}
#'   \item{save_conditionals}{If \code{TRUE} save the conditional estimates to
#'   a .csv file specified in \code{path}. Only valid for models with discrete
#'   or continuous mixing distributions. Default is \code{FALSE}.}
#'   \item{save_worker_info}{If \code{TRUE} save information about workers in
#'   .txt file specified in \code{path}. Only valid if \code{cores > 1}.
#'    Default is \code{FALSE}.}
#' }
#' 
#' @param save_opt_input A list of user specified estimation options
#' 
#' @return Returns a list of options with missing input values replaced by 
#' default values

set_save_opt_defaults <- function(save_opt_input) {
  save_opt <- list(
    path = file.path(getwd(), "model-01"),
    save_summary = FALSE,
    save_model_object = FALSE,
    save_hessian = FALSE,
    save_conditionals = FALSE,
    save_worker_info = FALSE
  )
  
  # Replace the non-specified values with default values
  save_opt[names(save_opt_input)] <- save_opt_input
  
  return(save_opt)
}
