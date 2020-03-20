  # Tidy the environment prior to loading packages ----
  cmdlR::tidy()
  rm(list = ls(all = TRUE))
  
  # Load the packages ----
  pkgs <- c("cmdlR")
  invisible(lapply(pkgs, require, character.only = TRUE))
  
  # Load and manipulate the data ----
  db <- cmdlR::data_petr_test
  
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
    cores = 10,
    robust_vcov = TRUE,
    print_level = 2
  )
  
  # Define the list of model options ----
  model_opt <- list(
    name = "MIXL model",
    description = "A simple MIXL model with 2 random parameters",
    id = "id",
    ct = "ct",
    choice = "choice",
    N = length(unique(db[["id"]])),
    S = length(unique(db[["ct"]])),
    J = 3L,
    mixing = TRUE,
    draws_type = "scrambled_sobol",
    R = 5000, 
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
  
  # Log likelihood function ----
  lik <- function(param, inputs) {
    # Attach the parameters and data  ----
    if (inputs$estim_opt$cores > 1) {
      attach_objects(list(param, db, draws))
      on.exit(detach_objects(list(param, db, draws)), add = TRUE)
    } else {
      attach_objects(list(param, inputs$db, inputs$draws))
      on.exit(detach_objects(list(param, inputs$db, inputs$draws)), add = TRUE)
    }
    
    # Calculate the indices ----
    N <- length(unique(get(inputs$model_opt$id)))
    S <- length(unique(get(inputs$model_opt$ct)))
    choice_var <- get(inputs$model_opt$choice)
    
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
    pr_chosen <- matrix(pr_chosen, nrow = S)
    
    # If the data is padded, we need to insert ones before taking the product
    index_na <- is.na(pr_chosen)
    pr_chosen[index_na][!is.nan(pr_chosen[index_na])] <- 1
    
    # Calculate the probability of the sequence
    pr_seq <- Rfast::colprods(pr_chosen)
    
    # Reshape the matrix such that each row is an individual and average over draws
    pr_seq <- matrix(pr_seq, nrow = N)
    pr_seq <- Rfast::rowmeans(pr_seq)
    
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
  # predictions <- predict(model)
  
  # Save the results ----
  store(model, inputs)
