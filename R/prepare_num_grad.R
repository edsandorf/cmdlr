#' Prepares the numerical gradient
#'
#' 'nloptr' and 'trustOptim' requires a gradient. This function is a wrapper
#' for numDeriv::grad() and prepares a high-precision numerical gradient to
#' use with 'nloptr' and 'trustOptim'.
#' 
#' @param lik Likelihood function
#' @param inputs List of inputs
#' @param workers A PSOCK cluster of workers
#' 
#' @return A numerical gradient wrapper function

prepare_num_grad <- function(lik, inputs, workers) {
  # Define the inner part (basically ll_func for single core)
  inner_num_grad <- function(param) {
    sum(log(lik(param, inputs)))
  }
  
  environment(inner_num_grad) <- new.env(parent = parent.env(environment(inner_num_grad)))
  
  if (inputs$estim_opt$cores > 1) {
    num_grad <- function(param) {
      grad_val <- do.call(
        rbind,
        parallel::clusterCall(
          cl = workers,
          param = param,
          fun = function (param) {
            numDeriv::grad(inner_num_grad, param, method = "Richardson")
          }
        )
      )
      Rfast::colsums(grad_val)
    }

  } else {
    num_grad <- function(param) {
      numDeriv::grad(inner_num_grad, param, method = "Richardson")
    }
  }
  
  # Return the numerical gradient
  cat(green$bold(symbol$tick), "  Numerical gradient\n")
  num_grad
}
