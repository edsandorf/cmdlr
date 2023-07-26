# Tidy the environment prior to loading packages ----
# cmdlR::tidy()
rm(list = ls(all = TRUE))

# Load the packages ----
pkgs <- c("cmdlr")
invisible(lapply(pkgs, require, character.only = TRUE))

# Define the list of estimation options ----
control <- list(
  optimizer = "bgw",
  method = "BFGS",
  cores = 4
)

# Define the list of model options ----
model_options <- list(
  name = "ICLV ordered",
  description = "An integrated choice and latent variable model with an ordered 
  measurement equation using the Apollo drug choice data",
  id = "ID",
  ct = "task",
  choice = "best",
  alt_avail = list(
    alt1 = 1,
    alt2 = 1,
    alt3 = 1, 
    alt4 = 1
  ),
  fixed = c("b_brand_Artemis", "b_country_USA", "b_char_standard"),
  param = list(
    b_brand_Artemis    = 0, 
    b_brand_Novum      = 0, 
    b_brand_BestValue  = 0, 
    b_brand_Supermarket= 0, 
    b_brand_PainAway   = 0, 
    b_country_CH       = 0, 
    b_country_DK       = 0, 
    b_country_USA      = 0, 
    b_country_IND      = 0, 
    b_country_RUS      = 0, 
    b_country_BRA      = 0, 
    b_char_standard    = 0, 
    b_char_fast        = 0, 
    b_char_double      = 0, 
    b_risk             = 0, 
    b_price            = 0,  
    lambda             = 1, 
    gamma_reg_user     = 0, 
    gamma_university   = 0, 
    gamma_age_50       = 0, 
    zeta_quality       = 1, 
    zeta_ingredient    = 1, 
    zeta_patent        = 1, 
    zeta_dominance     = 1, 
    tau_quality_1      =-2, 
    tau_quality_2      =-1, 
    tau_quality_3      = 1, 
    tau_quality_4      = 2, 
    tau_ingredients_1  =-2, 
    tau_ingredients_2  =-1, 
    tau_ingredients_3  = 1, 
    tau_ingredients_4  = 2, 
    tau_patent_1       =-2, 
    tau_patent_2       =-1, 
    tau_patent_3       = 1, 
    tau_patent_4       = 2, 
    tau_dominance_1    =-2, 
    tau_dominance_2    =-1, 
    tau_dominance_3    = 1, 
    tau_dominance_4    = 2
  ),
  mixing = TRUE,
  n_draws = 100,
  draws_type = "standard-halton",
  rpar = list(
    eta = "normal"
  )
)

# Likelihood function - returns the probability of the sequence of choices ----
ll <- function(param) {
  # Define a vector for sub-setting to one row per respondent
  subset_index <- task == 1
  
  # Define the latent variable(s) ----
  lv <- gamma_reg_user * regular_user + gamma_university * university_educated + gamma_age_50 * over_50 + eta
  lv_sub <- lv[subset_index, ]
  
  # Calculate the likelihood of the indicator(s) ----
  pr_indicators <- list(
    indicator_quality = ordered_logit(attitude_quality[subset_index], zeta_quality * lv_sub, c(tau_quality_1, tau_quality_2, tau_quality_3, tau_quality_4)),
    indicator_ingredients = ordered_logit(attitude_ingredients[subset_index], zeta_ingredient * lv_sub, c(tau_ingredients_1, tau_ingredients_2, tau_ingredients_3, tau_ingredients_4)),
    indicator_patent = ordered_logit(attitude_patent[subset_index], zeta_patent * lv_sub, c(tau_patent_1, tau_patent_2, tau_patent_3, tau_patent_4)),
    indicator_dominance = ordered_logit(attitude_dominance[subset_index], zeta_dominance * lv_sub, c(tau_dominance_1, tau_dominance_2, tau_dominance_3, tau_dominance_4))
  )
  pr_indicators <- Reduce("*", pr_indicators)
  
  # Define the list of utilities ---- 
  V <- list(
    alt1 = 
      b_brand_Artemis * (brand_1 == "Artemis") + 
      b_brand_Novum * (brand_1 == "Novum") + 
      b_country_CH * (country_1 == "Switzerland") + 
      b_country_DK * (country_1 == "Denmark") + 
      b_country_USA * (country_1 == "USA") +
      b_char_standard * (char_1 == "standard") +
      b_char_fast * (char_1 == "fast acting") + 
      b_char_double * (char_1 == "double strength") +
      b_risk * side_effects_1 + 
      b_price * price_1 +
      lambda * lv,
    alt2 =
      b_brand_Artemis * (brand_2 == "Artemis") + 
      b_brand_Novum * (brand_2 == "Novum") +
      b_country_CH * (country_2 == "Switzerland") +
      b_country_DK * (country_2 == "Denmark") +
      b_country_USA * (country_2 == "USA") +
      b_char_standard * (char_2 == "standard") + 
      b_char_fast * (char_2 == "fast acting") +
      b_char_double * (char_2 == "double strength") +
      b_risk * side_effects_2 +
      b_price * price_2 +
      lambda * lv,
    alt3 =  
      b_brand_BestValue * (brand_3 == "BestValue") +
      b_brand_Supermarket * (brand_3 == "Supermarket") + 
      b_brand_PainAway * (brand_3 == "PainAway") +
      b_country_USA * (country_3 == "USA") + 
      b_country_IND * (country_3 == "India") + 
      b_country_RUS * (country_3 == "Russia") + 
      b_country_BRA * (country_3 == "Brazil") +
      b_char_standard * (char_3 == "standard") +
      b_char_fast * (char_3 == "fast acting") +
      b_risk * side_effects_3 +
      b_price * price_3,
    alt4 =
      b_brand_BestValue * (brand_4 == "BestValue") + 
      b_brand_Supermarket * (brand_4 == "Supermarket") + 
      b_brand_PainAway * (brand_4 == "PainAway") +
      b_country_USA * (country_4 == "USA") +
      b_country_IND * (country_4 == "India") +
      b_country_RUS * (country_4 == "Russia") +
      b_country_BRA * (country_4 == "Brazil") +
      b_char_standard * (char_4 == "standard") +
      b_char_fast * (char_4 == "fast acting") +
      b_risk * side_effects_4 +
      b_price * price_4
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
  
  # Combine the model components. 
  pr_chosen <- pr_chosen
  
  # Calculate the product over the panel by reshaping to have rows equal to S
  pr_chosen <- matrix(pr_chosen, nrow = n_ct)
  
  # If the data is padded, we need to insert ones before taking the product
  index_na <- is.na(pr_chosen)
  pr_chosen[index_na][!is.nan(pr_chosen[index_na])] <- 1
  
  # Calculate the probability of the sequence
  pr_seq <- Rfast::colprods(pr_chosen)
  
  # Reshape the matrix such that each row is an individual and average over draws
  pr_seq <- matrix(pr_seq, nrow = n_id)
  
  # Multiply with the matrix of indicator likelihoods. NOTE: This will not work
  # correctly if indicator observations are at the choice task level. 
  pr_seq <- pr_seq * pr_indicators
  pr_seq <- Rfast::rowmeans(pr_seq)
  # ll <- log(pr_seq)
  ll <- pr_seq
  
  # Return the likelihood value
  return(ll)
}

# Load and manipulate the data ----
db <- apollo::apollo_drugChoiceData

# Prepare estimation environment ----
estim_env <- prepare(ll, db, model_options, control)

# Estimate the model ----
model <- estimate(ll, estim_env, model_options, control)

# Get a summary of the results ----
summary(model)
