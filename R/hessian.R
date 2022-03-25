#' Calculate the Hessian
#' 
#' The function is a wrapper around several methods for calculating the
#' numerical Hessian matrix. The function is meant for internal use and is not
#' exported. 
#' 
#' @param type A character string giving the method to use when approximating
#' the Hessian. One of: "numderiv" or "maxlik". 
#' @inheritParams estimate_model
#' @param method A method for the approximation of the Hessian. Only necessary
#' for the maxlik package
#' 
hessian <- function(type,
                    log_lik,
                    param_free,
                    param_fixed,
                    workers,
                    ll,
                    method) {
  
  # Define n_par
  n_par <- length(param_free)
  
  # Define the progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = 2 + 8 * (n_par * (n_par + 1) / 2),
    clear = FALSE, 
    width = 80
  )
  
  # Choose method for calculating Hessian. 
  hessian <- tryCatch(switch(type,
                             numderiv = numDeriv::hessian(func = log_lik,
                                                          x = param_free,
                                                          param_fixed = param_fixed,
                                                          workers = workers,
                                                          ll = ll, 
                                                          return_sum = TRUE,
                                                          pb = pb),
                             maxlik = maxLik::maxLik(logLik = log_lik,
                                                     start = param_free,
                                                     param_fixed = param_fixed,
                                                     workers = workers,
                                                     ll = ll,
                                                     return_sum = TRUE,
                                                     pb = pb, 
                                                     print.level = 0,
                                                     finalHessian = TRUE,
                                                     method = method,
                                                     iterlim = 2)[["hessian"]]),
                      error = function(e) {
                        return(NULL)
                      })
  
  if (!is.null(hessian)) {
    colnames(hessian) <- rownames(hessian) <- names(param_free)
  } 

  return(hessian)
}
