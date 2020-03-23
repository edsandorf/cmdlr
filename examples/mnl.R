# Tidy the environment prior to loading packages ----
cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlR")
invisible(lapply(pkgs, require, character.only = TRUE))

# Load and manipulate the data ----
db <- cmdlR::data_coral

# Define the list of saving options ----
save_opt <- list(
  path = file.path("outputs"),
  save_summary = FALSE,
  save_model_object = FALSE
)

# Define the list of summary options ----
summary_opt <- list(
  robust_se = FALSE 
)

# Define the list of estimation options ----
estim_opt <- list(
  optimizer = "maxlik",
  method = "BFGS",
  cores = 1,
  robust_vcov = TRUE,
  print_level = 2
)

# Define the list of model options ----
model_opt <- list(
  name = "MNL model",
  description = "A simple MNL model to use as an example.",
  id = "id",
  ct = "ct",
  choice = "choice",
  N = length(unique(db[["id"]])),
  S = length(unique(db[["ct"]])),
  J = 3L,
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

# Likelihood function - returns the probability of the sequence of choices ----
lik <- function(param, inputs) {
  # Attach the parameters and data  ----
  if (inputs$estim_opt$optimizer %in% c("nloptr")) {
    names(param) <- names(inputs$model_opt$param)
  }
  
  if (inputs$estim_opt$cores > 1) {
    attach_objects(list(param, db))
    on.exit(detach_objects(list(param, db)), add = TRUE)
  } else {
    attach_objects(list(param, inputs$db))
    on.exit(detach_objects(list(param, inputs$db)), add = TRUE)
  }

  # Calculate the indices ----
  N <- length(unique(get(inputs$model_opt$id)))
  S <- length(unique(get(inputs$model_opt$ct)))
  choice_var <- get(inputs$model_opt$choice)
  
  # Define the list of utilities ---- 
  V <- list(
    alt1 = b_cost * cost_1 + b_small * small_1 + b_large * large_1 + b_oil * oil_1 + b_fish * fish_1 + b_habitat + habitat_1,
    alt2 = b_cost * cost_2 + b_small * small_2 + b_large * large_2 + b_oil * oil_2 + b_fish * fish_2 + b_habitat + habitat_2,
    alt3 = b_cost * cost_3 + b_small * small_3 + b_large * large_3 + b_oil * oil_3 + b_fish * fish_3 + b_habitat + habitat_3
  )
  
  # Calculate the probabilities ----
  # Get the utility of the chosen alternative and subtract it from all utilities
  v_chosen <- Reduce("+", lapply(seq_along(V), function(j) {
    V[[j]] * (choice_var == j) 
  }))
  
  V <- lapply(V, function(v) v - v_chosen)
  
  # Calculate the exponent and sum of utilities
  exp_v <- lapply(V, function(v) exp(v))
  sum_v <- Reduce("+", exp_v)
  
  # Calculate the probability of the chosen alt
  pr_chosen <- 1 / sum_v
  
  # Calculate the product over the panel by reshaping to have rows equal to S
  pr_chosen <- matrix(pr_chosen, nrow = S)
  
  # If the data is padded, we need to insert ones before taking the product
  index_na <- is.na(pr_chosen)
  pr_chosen[index_na][!is.nan(pr_chosen[index_na])] <- 1
  
  # Calculate the probability of the sequence
  pr_seq <- Rfast::colprods(pr_chosen)
  
  # Return the likelihood value
  pr_seq
}

# Validate the model inputs ----
validate(lik, estim_opt, model_opt, save_opt, summary_opt)

# Prepare for estimation ----
inputs <- prepare(db, lik, estim_opt, model_opt, save_opt, summary_opt)

# Estimate the model ----
model <- estimate(inputs)

# Get a summary of the results ----
summarize(model, summary_opt)

# Collate results to a single file ----
#collate() 

# Make predictions ----
predictions <- predict(model)

# Save the results ----
store(model, inputs)
