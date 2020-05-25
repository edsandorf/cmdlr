#' Validates and sets default estimation options
#'
#' The function validates the user supplied list of save options and sets 
#' defaults for the missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
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
#' The function is intended for internal use only.
#'  
#' @param save_opt_input A list of user specified save options
#' 
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_save_opt <- function(save_opt_input) {
  save_opt <- list(
    name = "A model without a name",
    description = "Does not contain a description",
    path = NULL,
    save_summary = FALSE,
    save_model_object = FALSE,
    save_hessian = FALSE,
    save_vcov = FALSE,
    save_param = FALSE,
    save_worker_info = FALSE
  )

  # Replace the non-specified values with default values
  save_opt[names(save_opt_input)] <- save_opt_input
  
  # Check output location
  if (is.null(save_opt$path)) {
    message(blue$bold(symbol$info), paste0("  No output folder specified. Outputs are stored in the root of the current working directory: \"",
                                                   file.path(getwd()), "\""))
  }

  # Return the validated list of saving options
  message(green$bold(symbol$tick), "  save_opt()")
  save_opt
}
