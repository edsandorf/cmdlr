# Tidy the environment prior to loading packages ----
cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlR")
invisible(lapply(pkgs, require, character.only = TRUE))

# Load and manipulate the data ----
db <- apollo::apollo_modeChoiceData
db <- db[db$SP == 1, ]
db$ct <- rep(1:14, times = 500)

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
  description = "A simple MNL model using the Apollo dataset 'mode choice'.",
  id = "ID",
  ct = "ct",
  choice = "choice",
  N = length(unique(db[["ID"]])),
  S = length(unique(db[["ct"]])),
  J = 4L,
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
  
  # Define useful variables ----
  # Indices
  N <- length(unique(get(inputs$model_opt$id)))
  S <- length(unique(get(inputs$model_opt$ct)))
  
  # Choice variable
  choice_var <- get(inputs$model_opt$choice)
  
  # Define the list of utilities ---- 
  V <- list(
    alt1 = asc_car  + b_tt_car  * time_car                           + b_cost * cost_car,
    alt2 = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus,
    alt3 = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air  + b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3),
    alt4 = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail + b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  )
  
  # Alternative availability
  alt_availability <- list(
    alt1 = av_car,
    alt2 = av_bus,
    alt3 = av_air, 
    alt4 = av_rail
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
  exp_v <- mapply("*", exp_v, alt_availability, SIMPLIFY = FALSE)
  sum_v <- Reduce("+", exp_v)
  
  # Calculate the probability of the chosen alt
  # Returns zero if a non-available alternative is chosen
  chosen_alt_available <- Reduce("+", lapply(seq_along(V), function(j) {
    (choice_var == j) * alt_availability[[j]]
  }))
  pr_chosen <- chosen_alt_available / sum_v
  
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

# Search for starting values ----
# inputs$model_opt$param <- search_start_values(inputs)

# Estimate the model ----
model <- estimate(inputs)

# Get a summary of the results ----
summarize(model, inputs)

# Collate results to a single file ----
#collate() 

# Make predictions ----
# predictions <- predict(model)

# Save the results ----
# store(model, inputs)
