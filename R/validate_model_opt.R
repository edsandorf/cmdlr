#' Validates and sets default model options
#'
#' The function validates the user supplied list of model options and sets 
#' defaults for the missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
#'
#' @param model_opt_input A list of user specified model options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_model_opt <- function(model_opt_input) {
  # Set the defaults ----
  model_opt <- list(
    
  )
  
  # Replace the non-specified values with default values
  model_opt[names(model_opt_input)] <- model_opt_input
  
  # Check mixing options ----
  if (isTRUE(model_opt$mixing)) {
    # Check distribution type
    if (!all(tolower(unlist(model_opt$rpar)) %in% c("normal", "triangular", "uniform"))) {
      cat(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown distribution type. Distributions can be 'normal', 'uniform' or 'triangular'. Check that model_opt$rpar is correctly specified.")
    }
    
    # Check draws type
    if (!(tolower(model_opt$draws_type) %in% c("pseudo_random",
                                               "mlhs", "standard_halton",
                                               "scrambled_halton",
                                               "standard_sobol",
                                               "scrambled_sobol"))) {
      cat(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown type of draws specified. You can use: 'pseudo_random', 'mlhs', 'standard_halton', 'scrambled_halton', 'standard_sobol', 'scrambled_sobol'. Check that model_opt$draws_type is correctly specified.") 
    }
    
  }
  
  # Return the validated list of model options
  cat(green$bold(symbol$tick), "  model_opt()\n")
  model_opt
}
