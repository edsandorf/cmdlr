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
  optimizer = "maxLik",
  method = "BFGS",
  cores = 1,
  robust_vcov = TRUE,
  print_level = 2
)

# Define the list of model options ----
model_opt <- list(
  name = "LC model",
  description = "A simple LC model with 2 classes",
  id = "id",
  ct = "ct",
  choice = "choice",
  N = length(unique(db[["id"]])),
  S = length(unique(db[["ct"]])),
  J = 3L,
  fixed = c(),
  param = list(
    b_cost_1 = -0.2,
    b_small_1 = 0.2,
    b_large_1 = 0.4,
    b_oil_1 = -0.02,
    b_fish_1 = 0.1,
    b_habitat_1 = 0.9,
    b_cost_2 = -0.2,
    b_small_2 = 0.2,
    b_large_2 = 0.4,
    b_oil_2 = -0.02,
    b_fish_2 = 0.1,
    b_habitat_2 = 0.9,
    g_const_1 = 0.4
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
lik <- function(param, inputs) {
  # Attach the parameters and data  ----
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
  
  # Define the class probability functions ----
  P <- list(
    class1 = g_const_1,
    class2 = 0
  )
  
  # Define the list of utilities ---- 
  V <- list(
    V1 = list(
      alt1 = b_cost_1 * cost_1 + b_small_1 * small_1 + b_large_1 * large_1 + b_oil_1 * oil_1 + b_fish_1 * fish_1 + b_habitat_1 + habitat_1,
      alt2 = b_cost_1 * cost_2 + b_small_1 * small_2 + b_large_1 * large_2 + b_oil_1 * oil_2 + b_fish_1 * fish_2 + b_habitat_1 + habitat_2,
      alt3 = b_cost_1 * cost_3 + b_small_1 * small_3 + b_large_1 * large_3 + b_oil_1 * oil_3 + b_fish_1 * fish_3 + b_habitat_1 + habitat_3
    ),
    V2 = list(
      alt1 = b_cost_2 * cost_1 + b_small_2 * small_1 + b_large_2 * large_1 + b_oil_2 * oil_1 + b_fish_2 * fish_1 + b_habitat_2 + habitat_1,
      alt2 = b_cost_2 * cost_2 + b_small_2 * small_2 + b_large_2 * large_2 + b_oil_2 * oil_2 + b_fish_2 * fish_2 + b_habitat_2 + habitat_2,
      alt3 = b_cost_2 * cost_3 + b_small_2 * small_3 + b_large_2 * large_3 + b_oil_2 * oil_3 + b_fish_2 * fish_3 + b_habitat_2 + habitat_3
    )
  )
  
  # Calculate the choice and class probabilities ----
  # Calculate the probability of the sequence in each class
  pr_seq <- lapply(V, function(V_q) {
    # Get the utility of the chosen alternative and subtract it from all utilities 
    v_chosen <- Reduce("+", lapply(seq_along(V_q), function(j) {
      V_q[[j]] * (choice_var == j)
    }))
    
    V <- lapply(V_q, function(v) v - v_chosen)
    
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
  })
  
  # Matrix of N * Q
  pr_seq <- do.call(cbind, pr_seq)
  
  # Calculate the class probabilities
  exp_p <- lapply(P, function(p) exp(p))
  sum_p <- Reduce("+", exp_p)
  cls_pr <- lapply(exp_p, function(p) p / sum_p)
  
  # Average over individual since we have multiple observations per individual that do not vary across choice occasion
  cls_pr <- do.call(cbind, cls_pr)
  
  # If we have specified constants only then we need to make this the correct dim
  if (nrow(cls_pr) == 1) {
    cls_pr <- matrix(rep(cls_pr, each = N), nrow = N)
  } else {
    cls_pr <- matrix(cls_pr, nrow = S)
    cls_pr <- matrix(matrixStats::colMeans2(cls_pr, na.rm = TRUE), ncol = length(P))
  }

  # Return the probability of the sequence ----
  Rfast::rowsums(cls_pr * pr_seq)
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
