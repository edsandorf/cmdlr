# Tidy the environment prior to loading packages ----
# cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Define the list of estimation options ----
control <- list(
  optimizer = "ucminf",
  method = "BFGS",
  cores = 4
)

# Define the list of model options ----
model_options <- list(
  name = "MIXL model",
  description = "A simple MIXL model with 2 random parameters",
  id = "id",
  ct = "ct",
  choice = "choice",
  alt_avail = list(
    alt1 = 1,
    alt2 = 1, 
    alt3 = 1
  ),
  mixing = TRUE,
  draws_type = "scrambled-sobol",
  n_draws = 1000, 
  fixed = c(),
  param = list(
    b_alt1 = 0,
    b_alt2 = 0,
    mu_attr1 = 0,
    mu_attr2 = 0,
    sig_attr1 = 0,
    sig_attr2 = 0
  ),
  rpar = list(
    d_attr1 = "normal",
    d_attr2 = "normal"
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  
  # Define the random parameters - N*S x R matrices stacked ----
  b_attr1 <- mu_attr1 + sig_attr1 * d_attr1
  b_attr2 <- mu_attr2 + sig_attr2 * d_attr2
  
  # Define the list of utilities - N*S x R vectors ---- 
  V <- list(
    alt1 = b_alt1 + b_attr1 * attr1_1 + b_attr2 * attr2_1,
    alt2 = b_alt2 + b_attr1 * attr1_2 + b_attr2 * attr2_2,
    alt3 =          b_attr1 * attr1_3 + b_attr2 * attr2_3
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
  pr_chosen <- matrix(pr_chosen, nrow = n_ct)
  
  # If the data is padded, we need to insert ones before taking the product
  index_na <- is.na(pr_chosen)
  pr_chosen[index_na][!is.nan(pr_chosen[index_na])] <- 1
  
  # Calculate the probability of the sequence
  pr_seq <- Rfast::colprods(pr_chosen)
  
  # Reshape the matrix such that each row is an individual and average over draws
  pr_seq <- matrix(pr_seq, nrow = n_id)
  pr_seq <- Rfast::rowmeans(pr_seq)
  
  # Return the likelihood value
  ll <- log(pr_seq)
  
  return(-ll)
}

# Load and manipulate the data ----
db <- cmdlr::data_petr_test

# Prepare estimation environment ----
estim_env <- prepare(db, model_options, control)

# Estimate the model ----
model <- estimate(ll, estim_env, model_options, control)

# Get a summary of the results ----
summary(model)
