#-------------------------------------------------------------------------------
# Tidy the environment prior to loading packages 
#-------------------------------------------------------------------------------
cmdlR::tidy(clean = FALSE)

#-------------------------------------------------------------------------------
# Load the packages
#-------------------------------------------------------------------------------
pkgs <- c("cmdlR")
invisible(lapply(pkgs, require, character.only = TRUE))

#-------------------------------------------------------------------------------
# Load and manipulate the data
#-------------------------------------------------------------------------------
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
# Define the list of summary options
#-------------------------------------------------------------------------------
summary_opt <- list(
  robust_se = FALSE 
)

#-------------------------------------------------------------------------------
# Define the list of estimation options
#-------------------------------------------------------------------------------
estim_opt <- list(
  optimizer = "maxLik",
  method = "BFGS",
  cores = 1,
  robust_vcov = TRUE
)

#-------------------------------------------------------------------------------
# Define the list of model options
#-------------------------------------------------------------------------------
model_opt <- list(
  name = "MNL model",
  description = "A simple MNL model to use as an example.",
  id = c("id"),
  ct = c("ct"),
  choice = db$choice,
  fixed = c(),
  param = list(
    b_cost = -0.2,
    b_small = 0.2,
    b_large = 0.4,
    b_oil = -0.02,
    b_fish = 0.1,
    b_habitat = 0.9
  )
)

#-------------------------------------------------------------------------------
# Log likelihood function
#-------------------------------------------------------------------------------
log_lik <- function(param) {
  
  # Define the list of utilities 
  V <- list(
    alt1 = b_cost * cost_1 + b_small * small_1 + b_large * large_1 + b_oil * oil_1 + b_fish * fish_1 + b_habitat + habitat_1,
    alt2 = b_cost * cost_2 + b_small * small_2 + b_large * large_2 + b_oil * oil_2 + b_fish * fish_2 + b_habitat + habitat_2,
    alt3 = b_cost * cost_3 + b_small * small_3 + b_large * large_3 + b_oil * oil_3 + b_fish * fish_3 + b_habitat + habitat_3
  )
  
  # Calculate the probabilities
  probs <- mnl_probs(V)
  
  # Calculate the panel probabilities
  lik <- panel_probs(probs)
  
  # Return the log-likelihood value
  return(log(lik))
}

#-------------------------------------------------------------------------------
# Validate the model inputs
#-------------------------------------------------------------------------------
opts <- validate(estim_opt, model_opt, save_opt, summary_opt, log_lik)

#-------------------------------------------------------------------------------
# Prepare for estimation
#-------------------------------------------------------------------------------
inputs <- prepare(opts, db, log_lik)

#-------------------------------------------------------------------------------
# Estimate the model
#-------------------------------------------------------------------------------
model <- estimate(inputs)

#-------------------------------------------------------------------------------
# Get a summary of the results
#-------------------------------------------------------------------------------
summarize(model, summary_opt)

#-------------------------------------------------------------------------------
# Make predictions
#-------------------------------------------------------------------------------
predictions <- predict(model)

#-------------------------------------------------------------------------------
# Save the results
#-------------------------------------------------------------------------------
store()
