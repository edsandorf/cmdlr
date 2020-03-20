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
    name = "A model without a name",
    description = "Does not contain a description",
    id = NULL,
    ct = NULL,
    choice = NULL,
    N = length(unique(db[["id"]])),
    S = length(unique(db[["ct"]])),
    J = NULL,
    mixing = FALSE,
    draws_type = "scrambled_sobol",
    R = 10,
    fixed = NULL,
    param = NULL,
    rpar = NULL
  )
  
  # Replace the non-specified values with default values
  model_opt[names(model_opt_input)] <- model_opt_input
  
  # Check general model options ----
  if (is.null(model_opt$id) || is.null(model_opt$ct) || is.null(model_opt$choice)) {
    cat(red$bold(symbol$cross), "  model_opt().\n")
    stop("You must specify the id, ct and choice variables in model_opt.")
  }
  
  if (is.null(model_opt$J)) {
    cat(red$bold(symbol$cross), "  model_opt().\n")
    stop("You must specify the maximum number of alternatives faced by a given respondent 'J'.")
  }
  
  # Check mixing options ----
  if (isTRUE(model_opt$mixing)) {
    # Check draws type
    if (!(tolower(model_opt$draws_type) %in% c("pseudo_random",
                                               "mlhs", "standard_halton",
                                               "scrambled_halton",
                                               "standard_sobol",
                                               "scrambled_sobol"))) {
      cat(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown type of draws specified. You can use: 'pseudo_random', 'mlhs', 'standard_halton', 'scrambled_halton', 'standard_sobol', 'scrambled_sobol'. Check that model_opt$draws_type is correctly specified.") 
    }
    
    # Check if rpar is correctly specified
    if (is.null(model_opt$rpar)) {
      cat(red$bold(symbol$cross), "  model_opt().\n")
      stop("model_opt$rpar must be specified when mixing = TRUE.")
    } else {
      if (!is.list(model_opt$rpar)) {
        cat(red$bold(symbol$cross), "  model_opt().\n")
        stop("model_opt$rpar must be a list the length of the number of random parameters. The names of the list elements are used to call the specified draw within the log-likelihood function. ")
      }
    }
    
    # Check distribution type
    if (!all(tolower(unlist(model_opt$rpar)) %in% c("normal", "triangular", "uniform"))) {
      cat(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown distribution type. Distributions can be 'normal', 'uniform' or 'triangular'. Check that model_opt$rpar is correctly specified.")
    }
  }
  
  # Return the validated list of model options
  cat(green$bold(symbol$tick), "  model_opt()\n")
  model_opt
}
