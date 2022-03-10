#' Estimate the model 
#' 
#' The function is a wrapper around several methods for obtaining the optimum
#' using either maximization (maxLik) or minimization (ucminf). The function 
#' is meant for internal use and is not exported. 
#' 
#' @param type A character string giving the R pacakge to use for optimization.
#' One of: "maxlik" or "ucminf"
#' @param log_lik A log-likelihood expression prepared by 
#' \code{\link{prepare_log_lik}}
#' @param param_start The complete list of parameters at their starting values
#' (including fixed parameters)
#' @param param_free A list of freely estimated parameters
#' @param param_fixed A list of parameters that are held fixed at their 
#' starting values 
#' @param workers A list of workers for parallel computing
#' @param ll The 'raw' log likelihood function passed by the user
#' @param control A list of estimation controls passed to the optimizer 
#' 
#' @return A custom model object
estimate_model <- function(type,
                           log_lik,
                           param_start,
                           param_free,
                           param_fixed,
                           workers, 
                           ll, 
                           control) {
  
  model <- tryCatch(switch(type,
                           maxlik = estimate_maxlik(log_lik,
                                                    param_free,
                                                    param_fixed,
                                                    workers,
                                                    ll,
                                                    control),
                           ucminf = estimate_ucminf(log_lik,
                                                    param_free,
                                                    param_fixed,
                                                    workers,
                                                    ll,
                                                    control)),
                    error = function(e) {
                      return(NULL)
                    })
  
  # Currently not returning any useful debug information here
  if (is.null(model)) {
    return(NULL)
  }
  
  # Set convergence to TRUE if set to converge at starting values
  if (control[["iterlim"]] %in% c(0, 1)) {
    model[["converged"]] <- TRUE
  }
  
  # Additional model object elements
  model[["param_start"]] <- model[["param_final"]] <- param_start
  model[["param_fixed"]] <- param_fixed
  model[["param_final"]][names(param_free)] <- model[["param_free"]]
  model[["control"]] <- control
  
  # Set the new classes of the model object
  class(model) <- c("cmdlr", "list")
  
  # Return the model object
  return(model)
}

#' Estimate the model using the maxLik package
#' 
#' The function is a convenient wrapper around \code{\link{maxLik}}. 
#'
#' @inheritParams estimate_model
#'
#' @return A custom model object with the results fo the estimation.
estimate_maxlik <- function(log_lik,
                            param_free,
                            param_fixed,
                            workers,
                            ll,
                            control) {
  
  model_obj <- maxLik::maxLik(log_lik,
                              start = param_free,
                              param_fixed = param_fixed,
                              workers = workers,
                              ll = ll,
                              return_sum = FALSE,
                              pb = NULL,
                              method = control[["method"]],
                              finalHessian = FALSE,
                              tol = control[["tol"]],
                              gradtol = control[["gradtol"]],
                              reltol = control[["reltol"]], 
                              steptol = control[["steptol"]],
                              print.level = control[["print_level"]], 
                              iterlim = control[["iterlim"]])
  
  model <- list()
  model[["optimum"]] <- model_obj[["maximum"]]
  model[["message"]] <- model_obj[["message"]]
  model[["param_free"]] <- model_obj[["estimate"]]
  model[["convergence_code"]] <- model_obj[["code"]]
  
  # Check convergence
  if (control[["method"]] == "BFGS" && model_obj[["code"]] %in% c(0) ||
      control[["method"]] == "BHHH" && model_obj[["code"]] %in% c(2, 8) ||
      control[["method"]] == "NR" && model_obj[["code"]] %in% c(0, 1, 2)) {
    model[["converged"]] <- TRUE
    
  }   else {
    model[["converged"]] <- FALSE
  }
  
  return(model)
}

#' Estimate the model using the 'ucminf' package
#' 
#' The function is a convenient wrapper around \code{\link{ucminf}}. 
#' 
#' @inheritParams estimate_model
#' 
#' @return A custom model object with the results of the estimation
estimate_ucminf <- function(log_lik,
                            param_free,
                            param_fixed,
                            workers,
                            ll,
                            control) {

  model_obj <- ucminf::ucminf(par = param_free,
                              fn = log_lik,
                              hessian = 0,
                              param_fixed = param_fixed,
                              workers = workers,
                              ll = ll,
                              return_sum = TRUE,
                              pb = NULL, 
                              control = list(
                                grtol = control[["gradtol"]],
                                xtol = control[["steptol"]],
                                stepmax = control[["stepmax"]],
                                maxeval = control[["iterlim"]],
                                grad = control[["grad"]],
                                gradstep = control[["gradstep"]]
                              ))
  
  # Added a minus to make the fit calculations correct
  model <- list()
  model[["optimum"]] <- -model_obj[["value"]]
  model[["message"]] <- model_obj[["message"]]
  model[["param_free"]] <- model_obj[["par"]]
  model[["convergence_code"]] <- model_obj[["convergence"]]
  
  if (model_obj[["convergence"]] %in% c(1, 2, 3, 4)) {
    model[["converged"]] <- TRUE
  } else {
    model[["converged"]] <- FALSE
  }
  
  return(model)
}
