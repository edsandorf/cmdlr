#' Prepares the numerical gradient
#'
#' The optimization routines 'ucminf', 'nloptr' and 'trustOptim' requires the
#' user to supply a gradient. Writing an analytical gradient can be quite
#' cumbersome for very complex likelihood expressions. This function is a simple
#' wrapper around \code{numDeriv::grad()} and prepares a high-precision numerical
#' gradient that can be supplied directly to the optimizers that require one. 
#' Note that a numerical gradient is slower in calculation and less precise than 
#' an analytical gradient.
#' 
#' The function is meant for internal use only. 
#' 
#' @inheritParams prepare_log_lik
#' 
#' @return A high precision numerical gradient function

prepare_num_grad <- function(ll, estim_env, workers) {
  # Define the eval() wrapper for the numerical gradient
  inner_num_grad <- function(param) {
    invisible(
      lapply(seq_along(param), function(i) {
        assign(names(param[i]), param[[i]], envir = estim_env)
      })
    )
    sum(eval(body(ll), estim_env))
  }
  
  # environment(inner_num_grad) <- new.env(parent = parent.env(environment(inner_num_grad)))
  environment(inner_num_grad) <- environment(prepare_num_grad)
  
  # Define the numerical gradient
  num_grad <- function(param, converged) {
    if (is.null(workers)) {
      numDeriv::grad(inner_num_grad, param, method = "Richardson")
    } else {
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
  }
  
  # Return the numerical gradient
  message(green$bold(symbol$tick), "  Numerical gradient")
  num_grad
}
