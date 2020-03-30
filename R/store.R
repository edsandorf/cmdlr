#' Stores the results of the model to files
#'
#' This function stores various model outputs to easily accessible .csv files
#' and .rds objects. 
#'
#' @param model A model opbject returned by \code{estimate}
#' @param inputs A list of model inputs
#' 
#' @export

store <- function(model, inputs) {
  # Make sure that we do close file-writing if the function exits
  # on.exit(sink(), add = TRUE)
  
  # Get the save options for easy access
  save_opt <- inputs$save_opt
  
  # For storage purposes change the name of the model object to lower case and 
  # '-' separated. 
  model_name <- make_model_name(inputs$model_opt$name)
  
  # Check that files with the current names don't already exist to avoid over-writing them 
  file_list <- list.files(
    file.path(getwd(), save_opt$path),
    model_name
  )
  
  if (length(file_list) > 0) {
    cat(red$bold(symbol$cross), "  store().\n")
    stop("Files already exist in the specified output folder with the current model name. Either delete the files OR choose a new model name. This is to avoid over-writing results you want to keep. You can change the name directly using 'inputs$model_opt$name' and do not have to re-run any models.")
  }
  
  # Create the partial path 
  if (is.null(save_opt$path)) {
    path <- file.path(getwd(), model_name)
  } else {
    path <- file.path(getwd(), save_opt$path, model_name)
  }
  
  # Save model summary to .txt ----
  if (isTRUE(save_opt$save_summary)) {
    file_path <- paste0(path, "-summary.txt")
    sink(file_path)
    summarize(model, inputs)
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
}
