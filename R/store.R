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
#' @param model A model opbject returned by \code{estimate}
#' @inheritParams estimate
#' 
#' @examples
#' \dontrun{
#'     save_opt <- list(
#'     name = "MNL model",
#'     description = "A simple MNL model using the Apollo dataset 'mode choice'.",
#'     path = file.path("outputs"),
#'     save_summary = FALSE,
#'     save_model_object = FALSE
#'   )
#'   
#'   model <- estimate(ll, db, estim_opt, model_opt, summary_opt, save_opt)
#'   
#'   store(model, save_opt)
#' }
#' @export

store <- function(model, save_opt) {
  # Make sure that we do close file-writing if the function exits
  # on.exit(sink(), add = TRUE)
  
  # For storage purposes change the name of the model object to lower case and 
  # '-' separated. 
  model_name <- make_model_name(save_opt$name)
  model_name <- paste0(Sys.Date(), "-", model_name)
  
  # Create the partial path 
  if (is.null(save_opt$path)) {
    path <- getwd()
  } else {
    path <- file.path(getwd(), save_opt$path)
  }
  
  # Check that files with the current names don't already exist to avoid over-writing them 
  file_list <- list.files(
    path,
    model_name
  )
  
  if (length(file_list) > 0) {
    cat(yellow$bold(symbol$warning), paste0("  A file with the name ", model_name, " already exists in the specified folder. Adding numbering to the file name.\n"))
    unique_models <- unique(stringr::str_replace_all(file_list, "(-summary|-model-object|-hessian|-vcov|-param).*", ""))
    model_name <- paste0(model_name, "-", stringr::str_pad(as.character(length(unique_models) + 1), 3, side = "left", pad = "0"))
  }
  
  # Set the full path with partial model name
  path <- file.path(path, model_name)
  
  # Save model summary to .txt ----
  if (isTRUE(save_opt$save_summary)) {
    file_path <- paste0(path, "-summary.txt")
    sink(file_path)
    summarize(model)
    sink()
    cat(blue$bold(symbol$info), paste0("  Model summary saved to: \"", file_path, "\"\n"))
  }
  
  # Save model object to .rds ----
  if (isTRUE(save_opt$save_model_object)) {
    file_path <- paste0(path, "-model-object.rds")
    saveRDS(model, file_path)
    cat(blue$bold(symbol$info), paste0("  Model object saved to: \"", file_path, "\"\n"))
  }
  
  # Save hessian to a .csv file ----
  if (isTRUE(save_opt$save_hessian)) {
    file_path <- paste0(path, "-hessian.csv")
    utils::write.csv(model$hessian, file_path)
    cat(blue$bold(symbol$info), paste0("  Hessian matrix saved to: \"", file_path, "\"\n"))  
  }
  
  # Save variance-covariance matrix to .csv file ----
  if (isTRUE(save_opt$save_vcov)) {
    file_path <- paste0(path, "-vcov.csv")
    utils::write.csv(model$vcov, file_path)
    cat(blue$bold(symbol$info), paste0("  Variance-covariance matrix saved to: \"", file_path, "\"\n"))  
  }
  
  # Save parameters to a .csv file ----
  if (isTRUE(save_opt$save_param)) {
    file_path <- paste0(path, "-param.csv")
    utils::write.csv(model$coef, file_path)
    cat(blue$bold(symbol$info), paste0("  Final parameters saved to: \"", file_path, "\"\n"))  
  }
  
  # Save the choice analysis to a .csv file ----
  if (save_opt$save_choice_analysis) {
    file_path <- paste0(path, "-choice-analysis.csv")
    utils::write.csv(model$choice_analysis, file_path)
    cat(blue$bold(symbol$info), paste0("  Choice analysis saved to: \"", file_path, "\"\n"))  
  }
}
