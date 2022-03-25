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
  name = "MIXL uncorrelated LN",
  description = "A mixed logit model with uncorrelated log-normals",
  id = "ID",
  ct = "ct",
  choice = "choice",
  alt_avail = list(
    alt1 = 1,
    alt2 = 1
  ),
  fixed = c(),
  param = list(
    mu_log_b_tt    =-3,
    sigma_log_b_tt = 0.1,
    mu_log_b_tc    =-3,
    sigma_log_b_tc = 0.1,
    mu_log_b_hw    =-3,
    sigma_log_b_hw = 0.1,
    mu_log_b_ch    =-3,
    sigma_log_b_ch = 0.1
  ),
  mixing = TRUE,
  draws_type = "standard-halton",
  n_draws = 500, 
  rpar = list(
    draws_tt = "normal",
    draws_tc = "normal",
    draws_hw = "normal",
    draws_ch = "normal"
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define the random parameters - N*S x R matrices stacked ----
  b_tt <- -exp(mu_log_b_tt + sigma_log_b_tt * draws_tt)
  b_tc <- -exp(mu_log_b_tc + sigma_log_b_tc * draws_tc)
  b_hw <- -exp(mu_log_b_hw + sigma_log_b_hw * draws_hw)
  b_ch <- -exp(mu_log_b_ch + sigma_log_b_ch * draws_ch)
  
  # Define the list of utilities - N*S x R vectors ---- 
  V <- list(
    alt1 = b_tt * tt1 + b_tc * tc1 + b_hw * hw1 + b_ch * ch1,
    alt2 = b_tt * tt2 + b_tc * tc2 + b_hw * hw2 + b_ch * ch2
  )
  
  # Calculate the probabilities ----
  # Get the utility of the chosen alternative and subtract it from all utilities
  v_chosen <- Reduce("+", lapply(seq_along(V), function(j) {
    V[[j]] * (choice_var == j) 
  }))
  
  V <- lapply(V, function(v) v - v_chosen)
  
  # Calculate the exponent and sum of utilities
  exp_v <- lapply(V, function(v) exp(v))
  exp_v <- mapply("*", exp_v, alt_avail, SIMPLIFY = FALSE)
  sum_v <- Reduce("+", exp_v)
  
  # Calculate the probability of the chosen alt
  chosen_alt_available <- Reduce("+", lapply(seq_along(V), function(j) {
    (choice_var == j) * alt_avail[[j]]
  }))
  pr_chosen <- chosen_alt_available / sum_v
  
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
  ll <- log(pr_seq)
  
  # Return the likelihood value
  return(-ll)
}

# Load and manipulate the data ----
db <- apollo::apollo_swissRouteChoiceData
db$ct <- rep(1:9, times = 388)

# Prepare estimation environment ----
estim_env <- prepare(ll, db, model_options, control)

# Estimate the model ----
model <- estimate(ll, estim_env, model_options, control)

# Get a summary of the results ----
summary(model)

