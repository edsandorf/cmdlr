#' Extract the function value from the model object
#' 
#' @param object A 'cmdlr' model object
#' @param ... Additional arguments passed to the function
#' 
#' @return A single numeric value
#' 
#' @export
function_value <- function(object, ...) {
  return(
    object[["optimum"]]
  )
}

#' Extract the function values
#' 
#' @inheritParams function_value
#' 
#' @return A vector of function values at the optimum
#' 
#' @export
function_values <- function(object, ...) {
  return(
    object[["optimum_values"]]
  )
}

#' Extract the function value at zero from the model object
#' 
#' @inheritParams function_value
#' 
#' @return A single numeric value
#' 
#' @export
function_value_zero <- function(object, ...) {
  return(
    object[["optimum_at_zero"]]
  )
}

#' Extract the gradient of a model object
#'
#' @inheritParams function_value
#' 
#' @export
gradient <- function(object, ...) {
  return(
    object[["gradient"]]
  )
}

#' Extract the gradient observations from a model object
#'
#' @inheritParams function_value
#' 
#' @export
gradient_obs <- function(object, ...) {
  return(
    object[["gradient_obs"]]
  )
}

#' Extract free parameters from the model object
#'
#' @inheritParams function_value
#' 
#' @export
free_param <- function(object, ...) {
  return(
    object[["param_free"]]
  )
}

#' Extract fixed parameters from the model object
#'
#' @inheritParams function_value
#' 
#' @export
fixed_param <- function(object, ...) {
  return(
    object[["param_fixed"]]
  )
}

#' Extract starting parameters from the model object
#'
#' @inheritParams function_value
#' 
#' @export
start_param <- function(object, ...) {
  return(
    object[["param_start"]]
  )
}

#' Extract final parameters from the model object
#'
#' @inheritParams function_value
#' 
#' @export
final_param <- function(object, ...) {
  return(
    object[["param_final"]]
  )
}
