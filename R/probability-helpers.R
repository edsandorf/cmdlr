#' Calculate the utility of the chosen alternative
#' 
#' @param V A list of utilities
#' @param choice_var A a numeric vector with the chosen alternative by choice
#' task
#' 
#' @return A vector with the utility of the chosen alternative
#' 
#' @export
get_v_chosen <- function(V, choice_var) {
  return(
    Reduce("+", lapply(seq_along(V), function(j) {
      V[[j]] * (choice_var == j) 
    }))
  )
}

#' Take the difference between all utilities and the chosen alternative
#' 
#' @param V A list of utilities
#' @param v_chosen A vector of the chosen utilities
difference_utility <- function(V, v_chosen) {
  return(
    lapply(V, function(v) v - v_chosen)
  )
}

#' Take the exponent of V
#' 
#' Takes the exponent of V while at the same time restricts the utility of 
#' unavailable alternatives to 0. This ensures that summing of utilities is only
#' over alternatives in the consideration set. 
#' 
#' @param V A list of utilities
#' @param alt_avail A list of alternative availabilities
#' 
exp_v <- function(V, alt_avail) {
  # Calculate the exponent and sum of utilities
  V <- lapply(V, function(v) exp(v))
  
  # Restrict the utility of unavailable alternatives to zero
  V <- mapply("*", V, alt_avail, SIMPLIFY = FALSE)
  
  return(V)
}

#' Reshape the panel
#' 
#' A reshape function relying on R being column major and the data being stacked
#' and sorted correctly
#' 
#' @param x A matrix or vector
#' @param nrow Number of rows
#' 
#' @export
reshape_panel <- function(x, nrow) {
  return(
    matrix(x, nrow = nrow)
  )
}

#' Replace the NAs introduced by padding the data
#' 
#' Replaces the NA values introduced when padding the data. We need to replace
#' the NAs since no na.remove option exists for the Rfast package. See 
#' \code{\link{prepare_data}} for details
#' 
#' @param x A matrix or vector of chosen probabilities
#' 
#' @return A matrix or vector where NAs are replaced by 1
replace_na_padding <- function(x) {
  index_na <- is.na(x)
  x[index_na][!is.nan(x[index_na])] <- 1
  
  return(x)
}

#' Take product over the sequence of choices
#' 
#' Calculates the product across choice observations for the same individual.
#' The function makes a call to \code{\link{reshape_panel}} and 
#' \code{\link{replace_na_padding}} prior to taking the
#' product. 
#' 
#' @param x A matrix with the probabilities of the chosen alternative
#' @param n_ct The number of choice situations per individual 
#' 
#' @return A vector of sequence probabilities. If our model contains inter-
#' individual draws, then this vector is n_id * n_draws
#' 
#' @export
panel_product <- function(x, n_ct) {
  # Reshape x to have rows equal to n_ct
  x <- reshape_panel(x, n_ct)
  
  # Replace padding NA with 1 prior to passing to Rfast
  x <- replace_na_padding(x)
  
  # Return the column products of x
  return(Rfast::colprods(x))
}
