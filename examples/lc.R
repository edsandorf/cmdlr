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
  name = "LC model",
  description = "A simple LC model with 2 classes",
  id = "ID",
  ct = "ct",
  choice = "choice",
  alt_avail = list(
    alt1 = 1,
    alt2 = 1
  ),
  fixed = c("b_asc_2", "g_const_2"),
  param = list(
    b_asc_1 = 0,
    b_asc_2 = 0, 
    b_tt_1 = 0,
    b_tc_1 = 0,
    b_hw_1 = -0.0396, 
    b_ch_1 = -0.7624, 
    b_tt_2 = 0,
    b_tc_2 = 0,
    b_hw_2 = -0.0479, 
    b_ch_2 = -2.1725, 
    g_const_1 = 0.0329,
    g_const_2 = 0
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define the class probability functions ----
  P <- list(
    class1 = g_const_1,
    class2 = g_const_2
  )
  
  # Define the list of utilities ---- 
  V <- list(
    V1 = list(
      alt1 = b_asc_1 + b_tc_1 * tc1 + b_tt_1 * tt1 + b_hw_1 * hw1 + b_ch_1 * ch1,
      alt2 = b_asc_2 + b_tc_1 * tc2 + b_tt_1 * tt2 + b_hw_1 * hw2 + b_ch_1 * ch2
      
    ),
    V2 = list(
      alt1 = b_asc_1 + b_tc_2 * tc1 + b_tt_2 * tt1 + b_hw_2 * hw1 + b_ch_2 * ch1,
      alt2 = b_asc_2 + b_tc_2 * tc2 + b_tt_2 * tt2 + b_hw_2 * hw2 + b_ch_2 * ch2
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
    pr_chosen <- matrix(pr_chosen, nrow = n_ct)
    
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
    cls_pr <- matrix(rep(cls_pr, each = n_id), nrow = n_id)
  } else {
    cls_pr <- matrix(cls_pr, nrow = n_ct)
    cls_pr <- matrix(matrixStats::colMeans2(cls_pr, na.rm = TRUE), ncol = length(P))
  }
  
  # Return the probability of the sequence ----
  lik <- Rfast::rowsums(cls_pr * pr_seq)
  
  # Return the likelihood value
  ll <- log(lik)
  attributes(ll) <- list(
    pr_seq = pr_seq
  )
  
  return(-ll)
}

# Load and manipulate the data ----
db <- apollo::apollo_swissRouteChoiceData
db$ct <- rep(1:9, times = 388)

# Prepare estimation environment ----
estim_env <- prepare(db, model_options, control)

# Estimate the model ----
model <- estimate(ll, estim_env, model_options, control)

# Get a summary of the results ----
summary(model)
