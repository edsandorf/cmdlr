#' Take the panel product
#' 
#' Takes the product across the same choice situations for the same individual.
#' 
#' @param pr_chosen The probability of the chosen alternative returned from
#' \code{mnl_probs}
#' @param S The number of choie occasions per individual 
#' 
#' @return A vector that is N * DRAWS stacked by 

panel_product <- function(pr_chosen, S) {
  # Reshape to have rows equal to number of choice occasions
  pr_chosen <- matrix(pr_chosen, nrow = S)
  
  # If the data is padded, we need to insert ones before taking the product
  index_na <- is.na(pr_chosen)
  pr_chosen[index_na][!is.nan(pr_chosen[index_na])] <- 1
  
  # Calculate the probability of the sequence
  pr_seq <- Rfast::colprods(pr_chosen)
  return(pr_seq)
}
