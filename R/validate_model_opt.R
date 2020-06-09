#' Validates and sets default model options
#'
#' The function validates the user supplied list of model options and sets 
#' defaults for the missing options. If these options are essential, an error 
#' will occur and the program will stop. If the options are important, a
#' warning will be posted to the terminal and a default option will be used, 
#' but the program will continue.
#'
#' The function is intended for internal use only.
#' 
#' @param model_opt_input A list of user specified model options
#'
#' @return Returns a list of options with missing input values replaced by 
#' default values

validate_model_opt <- function(model_opt_input) {
  # Set the defaults ----
  model_opt <- list(
    id = NULL,
    ct = NULL,
    choice = NULL,
    N = NULL,
    S = NULL,
    J = NULL,
    mixing = FALSE,
    draws_type = "scrambled_sobol",
    R = 10,
    fixed = c(""),
    param = NULL,
    rpar = NULL
  )
  
  # Replace the non-specified values with default values
  model_opt[names(model_opt_input)] <- model_opt_input
  
  # Check general model options ----
  if (is.null(model_opt$id) || is.null(model_opt$ct) || is.null(model_opt$choice)) {
    message(red$bold(symbol$cross), "  model_opt().\n")
    stop("You must specify the id, ct and choice variables in model_opt.")
  }
  
  # if (is.null(model_opt$N) || is.null(model_opt$S) || is.null(model_opt$J)) {
  if (is.null(model_opt$J)) {
    message(red$bold(symbol$cross), "  model_opt().\n")
    # stop("You must specify the number of respondents 'N', maximum number of choice occasions 'S' and the maximum number of alternatives 'J'. These can be functions of your data.")
    stop("You must specify the maximum number of alternatives 'J'. These can be functions of your data.")
  }
  
  # Check the fixed parameter options ----
  # The implementation of fixed parameters is strongly influenced by the 'apollo package' and these tests are also implemented there.
  if (length(model_opt$fixed) > 0 && !is.character(model_opt$fixed)) {
    message(red$bold(symbol$cross), "  model_opt().\n")
    stop("'fixed' is a character vector with the names of the parameters to keep fixed at their starting values during estimation.")
  }
  
  if (length(unique(model_opt$fixed)) < length(model_opt$fixed)) {
    message(red$bold(symbol$cross), "  model_opt().\n")
    stop("'fixed' contains duplicate elements. Please check that all parameters are unique.")
  }
  
  if (!all(model_opt$fixed %in% names(model_opt$param))) {
    message(red$bold(symbol$cross), "  model_opt().\n")
    stop("Some parameters included in 'fixed' is not specified in the list of parameters. Please make sure that the parameters to fix are parameters in your model.")
  }
  
  # Check mixing options ----
  if (isTRUE(model_opt$mixing)) {
    # Check draws type
    if (!(tolower(model_opt$draws_type) %in% c("pseudo_random",
                                               "mlhs", "standard_halton",
                                               "scrambled_halton",
                                               "standard_sobol",
                                               "scrambled_sobol"))) {
      message(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown type of draws specified. You can use: 'pseudo_random', 'mlhs', 'standard_halton', 'scrambled_halton', 'standard_sobol', 'scrambled_sobol'. Check that model_opt$draws_type is correctly specified.") 
    }
    
    # Check if rpar is correctly specified
    if (is.null(model_opt$rpar)) {
      message(red$bold(symbol$cross), "  model_opt().\n")
      stop("model_opt$rpar must be specified when mixing = TRUE.")
    } else {
      if (!is.list(model_opt$rpar)) {
        message(red$bold(symbol$cross), "  model_opt().\n")
        stop("model_opt$rpar must be a list the length of the number of random parameters. The names of the list elements are used to call the specified draw within the log-likelihood function. ")
      }
    }
    
    # Check distribution type
    if (!all(tolower(unlist(model_opt$rpar)) %in% c("normal", "triangular", "uniform"))) {
      message(red$bold(symbol$cross), "  model_opt().\n")
      stop("Unknown distribution type. Distributions can be 'normal', 'uniform' or 'triangular'. Check that model_opt$rpar is correctly specified.")
    }
  }
  
  # Return the validated list of model options
  message(green$bold(symbol$tick), "  model_opt()")
  model_opt
}
