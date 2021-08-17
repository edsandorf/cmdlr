# Tidy the environment prior to loading packages ----
# cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Load and manipulate the data ----
db <- apollo::apollo_modeChoiceData
db <- db[db$SP == 1, ]
db$ct <- rep(1:14, times = 500)
db$mean_income <- mean(db$income)

# Define the list of saving options ----
save_opt <- list(
  name = "Nested logit model",
  description = "A simple NL model with 2 nests using the Apollo dataset 'mode choice'.",
  path = file.path("outputs"),
  save_summary = FALSE,
  save_model_object = FALSE,
  save_choice_analysis = FALSE
)

# Define the list of estimation options ----
estim_opt <- list(
  optimizer = "maxlik",
  method = "BFGS",
  cores = 1,
  calculate_hessian = TRUE,
  robust_vcov = TRUE,
  print_level = 2
)

# Define the list of model options ----
model_opt <- list(
  id = "ID",
  ct = "ct",
  choice = "choice",
  alt_avail = list(
    car = "av_car",
    bus = "av_bus",
    air = "av_air", 
    rail = "av_rail"
  ),
  fixed = c("asc_car", "b_no_frills"),
  param = list(
    asc_car = 0,
    asc_bus = 0,
    asc_air = 0,
    asc_rail = 0,
    asc_bus_shift_female = 0,
    asc_air_shift_female = 0,
    asc_rail_shift_female = 0,
    b_tt_car = 0,
    b_tt_bus = 0, 
    b_tt_air = 0,
    b_tt_rail = 0,
    b_tt_shift_business = 0,
    b_access = 0,
    b_cost = 0,
    b_cost_shift_business = 0,
    cost_income_elast = 0,
    b_no_frills = 0,
    b_wifi = 0,
    b_food = 0,
    lambda_pt = 0.95
  )
)

# Validate options ----
validated_options <- validate(estim_opt, model_opt, save_opt, db)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define the list of utilities
  asc_bus_value = asc_bus + asc_bus_shift_female * female
  asc_air_value = asc_air + asc_air_shift_female * female
  asc_rail_value = asc_rail + asc_rail_shift_female * female
  b_tt_car_value = b_tt_car + b_tt_shift_business * business
  b_tt_bus_value = b_tt_bus + b_tt_shift_business * business
  b_tt_air_value = b_tt_air + b_tt_shift_business * business
  b_tt_rail_value = b_tt_rail + b_tt_shift_business * business
  b_cost_value = ( b_cost + b_cost_shift_business * business ) * ( income / mean_income ) ^ cost_income_elast
  
  V <- list(
    car = asc_car + b_tt_car_value  * time_car                           + b_cost_value * cost_car,
    bus = asc_bus_value + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus,
    air = asc_air_value + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air  + b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3),
    rail = asc_rail_value     + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail + b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  )
  
  # Define the nesting structure
  nesting_parameters <- list( #nlNests
    level_1 = 1,
    level_2 = lambda_pt
  )
  
  # Note: The alternatives are nested so alt names in V must match nesting structure
  nesting_structure <- list( #nlStructure
    level_1 = c("car", "level_2"),
    level_2 = c("bus", "air", "rail")
  )
  
  # Calculate the probabilities ----
  # Get alt names before modifying V
  alt_names <- names(V)
  
  # Determine the list of ancestors
  ancestors <- vector(mode = "list", length = length(alt_names))
  names(ancestors) <- alt_names
  for (j in seq_along(ancestors)) {
    alt_j <- current <- alt_names[[j]]
    ancestors[[alt_j]] <- alt_j
    
    for (k in rev(seq_along(nesting_structure))) {
      if (current %in% nesting_structure[[k]]) {
        ancestors[[alt_names[[j]]]] <- c(ancestors[[alt_j]], names(nesting_structure)[[k]])
        current <- names(nesting_structure)[[k]]
      }
    }
  } 
  
  # Set the utility of the unavailable alternatives to 0
  alt_avail_use <- alt_avail
  V <- mapply("*", V, alt_avail_use, SIMPLIFY = FALSE)
  
  # Get the utility of the chosen alternative and subtract it from all utilities
  v_chosen <- Reduce("+", lapply(seq_along(V), function(j) {
    V[[j]] * (choice_var == j) 
  }))
  
  V <- lapply(V, "-", v_chosen)
  
  # Loop over nests to create new utility elements starting with the lowest level.
  # This follows the set-up in Apollo
  for (k in rev(seq_along(nesting_structure))) {
    # Get the names of the alternatives in nest k
    nest_k <- names(nesting_structure)[[k]]
    
    # Add nest_k to V
    V[[nest_k]] <- 0
    
    # The nest is unavailable if none of the options in the nest are available
    alt_avail_use[[nest_k]] <- 1L * (Reduce("+", alt_avail_use[nesting_structure[[k]]]) > 0L)
    
    # Loop over elements in the nest
    for (j in seq_along(nesting_structure[[k]])) {
      node_j <- nesting_structure[[k]][[j]]
      V[[nest_k]] <- V[[nest_k]] + alt_avail_use[[node_j]] * exp(V[[node_j]] / nesting_parameters[[nest_k]])
    }
    V[[nest_k]] <- nesting_parameters[[nest_k]] * log(V[[nest_k]])
  }
  
  # Calculate the probabilities
  pr_alt <- vector(mode = "list", length = length(alt_names))
  names(pr_alt) <- alt_names
  for (j in seq_along(pr_alt)) {
    pr_alt[[j]] <- 0
    ancestors_j <- ancestors[[alt_names[[j]]]]
    
    for (k in seq_len(length(ancestors_j) - 1)) {
      current_v <- V[[ancestors_j[[k]]]]
      next_v <- V[[ancestors_j[[k + 1]]]]
      pr_alt[[j]] <- pr_alt[[j]] + (current_v - next_v) / nesting_parameters[[ancestors_j[[k + 1]]]]
      
    }
  }
  
  pr_alt <- lapply(pr_alt, exp)
  
  # Is the chosen alt available?
  pr_alt <- mapply("*", pr_alt, alt_avail_use[seq_along(pr_alt)], SIMPLIFY = FALSE)
  
  pr_chosen <- Reduce("+", lapply(seq_along(pr_alt), function(j) {
    pr_alt[[j]] * (choice_var == j) 
  }))
  
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
  
  return(ll)
}

# Prepare inputs ----
prepared_inputs <- prepare(db, ll, validated_options)

# Estimate the model ----
model <- estimate(ll, prepared_inputs, validated_options)

# Get a summary of the results ----
summarize(model)
