#' Take the panel product
#' 
#' Calculates the product across choice observations for the same individual
#' 
#' @param probability The probability of the chosen alternative returned from
#' \code{mnl_probs}
#' @param n_ct The number of choie situations per individual 
#' 
#' @return A vector of sequence probabilities. If our model contains inter-
#' individual draws, then this vector is n_id * n_draws
#' 
#' @export
panel_product <- function(probability, n_ct) {
  # Reshape to have rows equal to number of choice occasions
  pr_chosen <- matrix(probability, nrow = n_ct)
  
  # If the data is padded, we need to insert ones before taking the product
  index_na <- is.na(probability)
  probability[index_na][!is.nan(probability[index_na])] <- 1L
  
  # Calculate the probability of the sequence
  pr_seq <- Rfast::colprods(probability)
  return(pr_seq)
}
