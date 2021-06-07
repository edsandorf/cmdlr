# Tidy the environment prior to loading packages ----
# cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Load and manipulate the data ----
db <- apollo::apollo_timeUseData
db$t_outside <- rowSums(db[, c("t_a01", "t_a06", "t_a10", "t_a11", "t_a12")])
db$t_leisure <- rowSums(db[, c("t_a07", "t_a08", "t_a09")])

# Create a constant and a new choice task variable 
db$constant <- 1
db$ct <- Reduce("c", lapply(unique(db$indivID), function(x) seq_len(length(which(db$indivID == x)))))

# Define the list of saving options ----
save_opt <- list(
  name = "MDCEV with outside good",
  description = "A MDCEV model with an outside good using the Apollo time use data to check code performance. Check out the Apollo package",
  path = file.path("outputs"),
  save_summary = FALSE,
  save_model_object = FALSE,
  save_choice_analysis = FALSE
)

# Define the list of estimation options ----
estim_opt <- list(
  optimizer = "ucminf",
  method = "BFGS",
  cores = 1,
  calculate_hessian = TRUE,
  robust_vcov = TRUE,
  print_level = 2
)

# Define the list of model options ----
# NOTE: The outside good must be the first good and it is always available and 
# consumed.
model_opt <- list(
  id = "indivID",
  ct = "ct",
  choice = "weekend", # This is not used for the MDCEV model, but needs to be specified
  alt_avail = list(
    outside = 1,
    work = 1,
    school = 1, 
    shopping = 1,
    private = 1,
    leisure = 1
  ),
  fixed = c("delta_outside", "sigma"),
  param = list(
    alpha_base         = 0,
    gamma_work         = 1,
    gamma_school       = 1,
    gamma_shopping     = 1,
    gamma_private      = 1,
    gamma_leisure      = 1,
    delta_outside = 0,
    delta_work         = 0,
    delta_school       = 0,
    delta_shopping     = 0,
    delta_private      = 0,
    delta_leisure      = 0,
    delta_work_FT      = 0,
    delta_work_wknd    = 0,
    delta_school_young = 0,
    delta_leisure_wknd = 0,
    sigma              = 0
  )
)

# Validate options ----
validated_options <- validate(estim_opt, model_opt, save_opt, db)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define the list of utilities ---- 
  # NOTE: The utility of the outside good must be equal to 0. The constant (created in the data)
  # ensures that all filled-in rows are equal to NA
  V <- list(
    outside = delta_outside * constant,  
    work = delta_work + delta_work_FT * occ_full_time + delta_work_wknd * weekend,
    school = delta_school + delta_school_young * (age<=30),
    shopping = delta_shopping * constant,
    private = delta_private * constant,
    leisure = delta_leisure + delta_leisure_wknd * weekend
  )
  
  # Define consumption / continuous choice ----
  x_star <- list(
    outside = t_outside / 60,
    work = t_a02 / 60,
    school = t_a03 / 60,
    shopping = t_a04 / 60,
    private = t_a05 / 60,
    leisure = t_leisure / 60
  )
  
  # Define prices / cost ----
  # Price of outside good (p_1) must be set equal to 1 for term 3 to calculate correctly
  p_star <- list(
    outside  = 1, 
    work     = 1, 
    school   = 1, 
    shopping = 1, 
    private  = 1,
    leisure  = 1
  )
  
  # Define alpha ----
  alpha <- list(
    outside = 1 / (1 + exp(-alpha_base)),
    work = 1 / (1 + exp(-alpha_base)),
    school = 1 / (1 + exp(-alpha_base)),
    shopping = 1 / (1 + exp(-alpha_base)),
    private = 1 / (1 + exp(-alpha_base)),
    leisure = 1 / (1 + exp(-alpha_base))
  )
  
  # Define gamma ----
  gamma <- list(
    outside = 1, # This is just here to make the indexing work out. It's never actually used
    work = gamma_work,
    school = gamma_school,
    shopping = gamma_shopping,
    private = gamma_private,
    leisure = gamma_leisure
  )
  
  # Define sigma ----
  sigma <- exp(sigma)
  
  # Calculate the probabilities ----
  # Define the discrete choice indicator
  discrete_choice <- lapply(x_star, ">", 0)
  
  # Define the total number of chosen alternatives M
  total_chosen <- Reduce("+", discrete_choice)
  
  # Discrete choice available
  discrete_choice_avail <- mapply("*", discrete_choice, alt_avail, SIMPLIFY = FALSE)
  
  # Calculate V
  V <- lapply(seq_along(V), function(i) {
    if(i == 1) {
      # Only works if the outside good is the first alternative AND the utility is set to zero
      V[[i]] + ((alpha[[i]] - 1) * log(x_star[[i]])) * alt_avail[[i]]
    } else {
      V[[i]] + ((alpha[[i]] - 1) * log((x_star[[i]] / gamma[[i]]) + 1) - log(p_star[[i]])) * alt_avail[[i]]
    }
  })
  
  # Taking the natural logarithm of P() makes it easier to program the log-likelihood
  # function. Equation 33 in Bhat (2008). P() then becomes 5 terms that can be summed
  # First term
  term_1 <- (1 - total_chosen) * log(sigma)
  
  # Second term
  # Calculate log_fi - the first alternative is the outside good
  log_fi <- lapply(seq_along(alpha), function(i) {
    if (i == 1) {
      log(1 - alpha[[i]]) - log(x_star[[i]])
    } else {
      log(1 - alpha[[i]]) - log(x_star[[i]] + gamma[[i]])
    }
  })
  
  term_2 <- Reduce("+", lapply(seq_along(log_fi), function(i) {
    # Use discrete choice and alt_avail to restrict to zero if not chosen
    log_fi[[i]] * discrete_choice_avail[[i]]
  }))
  
  # Third term
  term_3 <- log(Reduce("+", lapply(seq_along(log_fi), function(i) {
    (p_star[[i]] / exp(log_fi[[i]])) * discrete_choice_avail[[i]]
  })))
  
  # Fourth term
  term_4_1 <- Reduce("+", lapply(seq_along(V), function(i) {
    (V[[i]] / sigma) * discrete_choice_avail[[i]]
  }))
  
  term_4_2 <- log(Reduce("+", lapply(seq_along(V), function(i) {
    exp(V[[i]] / sigma) * alt_avail[[i]]
  })))
  
  term_4 <- term_4_1 - (total_chosen * term_4_2)
  
  # Fith term
  term_5 <- lfactorial(total_chosen - 1)
  
  # Add the terms of the log-likelihood expression
  pr_chosen <- exp(term_1 + term_2 + term_3 + term_4 + term_5)
  
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

# Prepare inputs ----
prepared_inputs <- prepare(db, ll, validated_options)

# Estimate the model ----
model <- estimate(ll, prepared_inputs, validated_options)

# Get a summary of the results ----
summarize(model)
