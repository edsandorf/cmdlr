#-------------------------------------------------------------------------------
#Load the packages
#-------------------------------------------------------------------------------
pkgs <- c("cmdlR")
invisible(lapply(pkgs, require, character.only = TRUE))

#-------------------------------------------------------------------------------
#Load and manipulate the data
#-------------------------------------------------------------------------------
# path <- file.path("data", "data_coral.rda")
# db <- load(path)
db <- cmdlR::data_coral

#-------------------------------------------------------------------------------
# Define the list of saving options
#-------------------------------------------------------------------------------
save_opt <- list(
  path = file.path("Outputs", "mnl-01"),
  save_summary = TRUE,
  save_model_object = TRUE,
  save_hessian = FALSE,
  save_worker_info = FALSE
)

#-------------------------------------------------------------------------------
# Define the list of estimation options
#-------------------------------------------------------------------------------
estim_opt <- list(
  cores = 1
)

#-------------------------------------------------------------------------------
# Define the list of model options
#-------------------------------------------------------------------------------
model_opt <- list(
  name = "MNL model",
  description = "A simple MNL model to use as an example.",
  id = c("id"),
  ct = c("ct"),
  alt = c(),
  choice = c(),
  fixed = c(),
  param = c(
    b_cost = 0,
    b_small = 0,
    b_large = 0,
    b_oil = 0,
    b_fish = 0,
    b_habitat = 0
  )
)

#-------------------------------------------------------------------------------
# Log likelihood function
#-------------------------------------------------------------------------------
log_lik <- function() {
  
  # Define the list of utilities 
  V <- list(
    alt1 = b_cost * cost_1 + b_small * small_1 + b_large * large_1 + b_oil * oil_1 + b_fish * fish_1 + b_habitat + habitat_1,
    alt2 = b_cost * cost_2 + b_small * small_2 + b_large * large_2 + b_oil * oil_2 + b_fish * fish_2 + b_habitat + habitat_2,
    alt3 = b_cost * cost_3 + b_small * small_3 + b_large * large_3 + b_oil * oil_3 + b_fish * fish_3 + b_habitat + habitat_3
  )
  
  # Calculate the probabilities
  probs <- mnl_probs(V)
  
  # Calculate the panel probabilities
  
  # Return the log-likelihood value
  return()
}

#-------------------------------------------------------------------------------
# Validate the model inputs
#-------------------------------------------------------------------------------
model_inputs <- validate(estim_opt, model_opt, save_opt, db, log_lik)

#-------------------------------------------------------------------------------
# If we are using parallel estimation, we need to prepare the workers
#-------------------------------------------------------------------------------
if (estim_opt$cores > 1) {
  prepare_parallel()
}

#-------------------------------------------------------------------------------
# Estimate the model
#-------------------------------------------------------------------------------
model_tmp <- estimate(log_lik, estim_opt)
