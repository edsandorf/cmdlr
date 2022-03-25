# Tidy the environment prior to loading packages ----
# cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Define the list of estimation options ----
control <- list(
  optimizer = "ucminf",
  method = "BFGS"
)

# Define the list of model options ----
model_options <- list(
  name = "MNL model",
  description = "A simple MNL model using the Apollo dataset 'mode choice'.",
  id = "ID",
  ct = "SP_task",
  choice = "choice",
  alt_avail = list(
    alt1 = "av_car",
    alt2 = "av_bus",
    alt3 = "av_air", 
    alt4 = "av_rail"
  ),
  fixed = c("asc_car", "b_no_frills"),
  param = list(
    asc_car = 0,
    asc_bus = 0,
    asc_air = 0,
    asc_rail = 0,
    b_tt_car = 0,
    b_tt_bus = 0, 
    b_tt_air = 0,
    b_tt_rail = 0,
    b_access = 0,
    b_cost = 0,
    b_no_frills = 0,
    b_wifi = 0,
    b_food = 0
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define the list of utilities ---- 
  V <- list(
    alt1 = asc_car  + b_tt_car  * time_car                           + b_cost * cost_car,
    alt2 = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus,
    alt3 = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air  + b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3),
    alt4 = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail + b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  )
  
  # Calculate the probabilities ----
  # Get the utility of the chosen alternative and subtract it from all utilities
  v_chosen <- Reduce("+", lapply(seq_along(V), function(j) {
    V[[j]] * (choice_var == j) 
  }))
  
  V <- lapply(V, function(v) v - v_chosen)
  
  # Calculate the exponent and sum of utilities
  exp_v <- lapply(V, function(v) exp(v))
  # Restrict the utility of unavailable alternatives to 0
  exp_v <- mapply("*", exp_v, alt_avail, SIMPLIFY = FALSE)
  sum_v <- Reduce("+", exp_v)
  
  # Calculate the probability of the chosen alt
  # Returns zero if a non-available alternative is chosen
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
  
  # Return the likelihood value
  ll <- log(pr_seq)
  attributes(ll) <- list(
    pr_chosen = pr_chosen
  )
  
  return(-ll)
}

# Load and manipulate the data ----
db <- apollo::apollo_modeChoiceData
db <- db[db$SP == 1, ]

# Prepare estimation environment ----
estim_env <- prepare(ll, db, model_options, control)

# Search for starting values ----
start_values <- search_start_values(ll,
                                    estim_env, 
                                    model_options,
                                    control,
                                    type = "simple")

model_options[["param"]] <- as.list(start_values[1, ])

# Estimate the model ----
model <- estimate(ll, estim_env, model_options, control)

# Get a summary of the results ----
summary(model)

