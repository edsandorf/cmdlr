#' Collate different model objects into a single .csv file
#'
#' @description
#' The function looks in the output folder and collects all model objects given
#' by the \code{pattern} and collates them into a single .csv file for easy
#' comparison. To make comparison easier, the function will create a vector of
#' unique parameter names and match parameters from all models to the vector.
#' This ensures that the same parameters from different models are in the same
#' row in the output file.
#'
#' @param path The relative path (from working directory) to the folder where 
#' the outputs are stored. Default is NULL, which means outputs are found in 
#' the root working directory
#' @param pattern A regular expression. Only file names which match the regular
#' expression will be returned.
#' @param digits An integer giving the number of digits to round results
#'
#' @export
collate <- function(path = NULL, pattern = NULL, digits = 4) {
  # Check if path is specified
  if (is.null(path)) {
    path <- getwd()
  } else {
    path <- file.path(getwd(), path)
  }
  
  # Create a list of files that match the pattern
  file_list <- list.files(
    path,
    pattern,
    full.names = TRUE
  )
  
  # Filter the list to only return .rds
  file_list <- file_list[grep("*.rds", file_list)]
  
  # If there are no .rds files returned, then stop
  if (length(file_list) < 1) {
    stop(
      "Can only collate results from .rds files. Make sure that you have stored 
      the model object in the location specified in path and that your pattern, 
      if applied, does match at least one of the model objects .rds"
    )
  }
  
  # Lapply through the vector to read in the model objects
  models <- lapply(file_list, function(x) {
    readRDS(x)
  })
  
  # Extract the names and create a matrix to use for matching
  # This works, but it does it alphabetically and numerically - extension here?
  row_names <- c(
    unique(sort(Reduce(c, lapply(models, function(x) names(x$param_final))))),
    "name", "nobs", "K", "ll", "adj_rho_sqrd", "bic", "aic", "draws_type", "R"
  )
  
  results <- matrix(NA, nrow = length(row_names), ncol = (length(models) * 2),
                    dimnames = list(
                      row_names,
                      rep(c("est", "se"), times = length(models))
                    ))
  
  # Loop over the models
  for (i in seq_along(models)) {
    # Create indices 
    i_est <- i + (i - 1)
    i_se <- i_est + 1
    
    # Extract the coefficients, calculate the standard errors
    coef_tmp <- round(models[[i]][["param_final"]], digits)
    se_tmp <- round(sqrt(diag(models[[i]][["vcov"]])), digits)
    results[names(coef_tmp), i_est] <- coef_tmp
    results[names(se_tmp), i_se] <- se_tmp
    
    # Extrac the model name and characteristics and append to the matrix
    results["name", i_est] <- models[[i]][["name"]]
    results["nobs", i_est] <- models[[i]][["nobs"]]
    results["K", i_est] <- length(models[[i]][["param_final"]])
    results["ll", i_est] <- round(models[[i]][["ll"]], digits)
    results["adj_rho_sqrd", i_est] <- round(models[[i]][["adj_rho_sqrd"]], digits)
    results["bic", i_est] <- round(models[[i]][["bic"]], digits)
    results["aic", i_est] <- round(models[[i]][["aic"]], digits)
    results["draws_type", i_est] <- models[[i]][["draws_type"]]
    results["R", i_est] <- models[[i]][["R"]]
  }
  
  
  # Write the .csv file to the outputs folder
  file_path <- paste0(path, "-collated-results.csv")
  utils::write.csv(results, file_path)
  cli::cli_alert_info(paste0("The results are saved to: ", file_path))
}
