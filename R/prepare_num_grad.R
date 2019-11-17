#' Prepares the numerical gradient
#'
#' 'nloptr' and 'trustOptim' requires a gradient. This function is a wrapper
#' for numDeriv::grad() and prepares a high-precision numerical gradient to
#' use with 'nloptr' and 'trustOptim'.
#' 
#' @return A numerical gradient wrapper function

prepare_num_grad <- function() {
  
}
