#' Extract the control list
#'
#' @inheritParams get_function_value
#' 
#' @export
get_control <- function(object, ...) {
  return(
    object[["control"]]
  )
}

#' Extract the convergence of the model
#'
#' @inheritParams get_function_value
#' 
#' @export
get_convergence <- function(object, ...) {
  return(
    object[["convergence"]]
  )
}

#' Extract the convergence code 
#'
#' @inheritParams get_function_value
#' 
#' @export
get_convergence_code <- function(object, ...) {
  return(
    object[["convergence_code"]]
  )
}

#' Extract the description of the model 
#'
#' @inheritParams get_function_value
#' 
#' @export
get_description <- function(object, ...) {
  return(
    object[["description"]]
  )
}

#' Extract the gradient of a model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_gradient <- function(object, ...) {
  return(
    object[["gradient"]]
  )
}

#' Extract the gradient observations from a model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_gradient_obs <- function(object, ...) {
  return(
    object[["gradient_obs"]]
  )
}

#' Extract the Hessian
#'
#' @inheritParams get_function_value
#' 
#' @export
get_hessian <- function(object, ...) {
  return(
    object[["hessian"]]
  )
}

#' Extract the convergence message
#'
#' @inheritParams get_function_value
#' 
#' @export
get_convergence_message <- function(object, ...) {
  return(
    object[["message"]]
  )
}

#' Extract the name of the model
#'
#' @inheritParams get_function_value
#' 
#' @export
get_name <- function(object, ...) {
  return(
    object[["name"]]
  )
}

#' Extract the function value from the model object
#' 
#' @param object A 'cmdlr' model object
#' @param ... Additional arguments passed to the function
#' 
#' @return A single numeric value
#' 
#' @export
get_function_value <- function(object, ...) {
  return(
    object[["optimum"]]
  )
}

#' Extract the function values
#' 
#' @inheritParams get_function_value
#' 
#' @return A vector of function values at the optimum
#' 
#' @export
get_function_values <- function(object, ...) {
  return(
    object[["optimum_values"]]
  )
}

#' Extract the function value at zero from the model object
#' 
#' @inheritParams get_function_value
#' 
#' @return A single numeric value
#' 
#' @export
get_function_value_zero <- function(object, ...) {
  return(
    object[["optimum_at_zero"]]
  )
}

#' Extract free parameters from the model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_param_free <- function(object, ...) {
  return(
    object[["param_free"]]
  )
}

#' Extract fixed parameters from the model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_param_fixed <- function(object, ...) {
  return(
    object[["param_fixed"]]
  )
}

#' Extract starting parameters from the model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_param_start <- function(object, ...) {
  return(
    object[["param_start"]]
  )
}

#' Extract final parameters from the model object
#'
#' @inheritParams get_function_value
#' 
#' @export
get_param_final <- function(object, ...) {
  return(
    object[["param_final"]]
  )
}

#' Extract start time for estimation
#'
#' @inheritParams get_function_value
#' 
#' @export
get_estimation_start <- function(object, ...) {
  return(
    object[["estimation_start"]]
  )
}

#' Extract end time for estimation
#'
#' @inheritParams get_function_value
#' 
#' @export
get_estimation_end <- function(object, ...) {
  return(
    object[["estimation_end"]]
  )
}

#' Extract the model_frame
#' 
#' @inheritParams get_function_value
#' 
#' @export
get_model_frame <- function(object, ...) {
  return(
    object[["model_frame"]]
  )
}
