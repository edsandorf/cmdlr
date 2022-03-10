#' S3 Generic for model summary
#'
#' A generic summary function for a model object of class 'cmdlr'
#' 
#' @inheritParams vcov.cmdlr
#' 
#' @method summary cmdlr
#' 
#' @export
summary.cmdlr <- function(object, robust = TRUE, ...) {
  cat("---------------------------------------------------------------------\n")
  cat("Convergence message   ", object[["message"]], "\n")
  cat("Convergence criteria  ", convergence_criteria(object), "\n")
  cat(paste0("Estimation started    ", object[["time_start"]], "\n"))
  cat(paste0("Estimation completed  ", object[["time_end"]], "\n"))
  cat("\n\n")
  
  cat("----------------------------- Model fit -----------------------------\n")
  print(glance(object) %>%
          tidyr::pivot_longer(tidyselect::everything(),
                              names_to = "stat", 
                              values_to = "value"))
  cat("\n\n")
  
  cat("----------------------- Parameter information -----------------------\n")
  print(parameter_info(object))
  cat("\n\n")
  
  cat("----------------------------- Parameters-----------------------------\n")
  print(tidy(object, robust = robust) %>% 
          dplyr::mutate(
            star = stars(.data$p.value)
          ))
  cat("---------------------------------------------------------------------\n")
  cat("'***' - 0.1% level, '**' - 1% level, '*' - 5% level, '.' - 10% level\n")
  cat(paste0("Reported using ",
             ifelse(robust, "robust", "normal"),
             "standard errors\n"))
  cat("\n\n")
  
}

#' Summary of parameter info
#' 
#' Creating a summary of parameter information including starting values, 
#' final values, and Eigen values of the Hessian matrix.
#' 
#' @param object A 'cmdlr' model object
#' @param ... Additional arguments passed to the function
#' 
#' @return A tidy [tibble::tibble()] summarizing the parameter information
#' 
#' @export
parameter_info <- function(object, ...) {
  
  eigen_values <- grad <- rep(NA, length(start_param(object)))
  names(eigen_values) <- names(grad) <- names(start_param(object))
  
  # Calculate the Eigen values of the model object
  eigen_values[names(free_param(object))] <- tryCatch({
    eigen(object[["hessian"]])[["values"]]
  },
  error = function(e) {
    return(
      rep(NA, nrow(object[["hessian"]]))
    )
  })
  
  # Fix gradient 
  grad[names(free_param(object))] <- object[["gradient"]]
  
  param_info <- tibble::tibble(
    term = names(start_param(object)),
    start = start_param(object),
    final = final_param(object),
    diff = .data$final - .data$start,
    grad = grad,
    eigen = eigen_values
  )
  
  return(param_info)
}

#' Calculate p-values
#' 
#' A convenient function to calculate p-values for **t-statistics**. By default
#' p-values are calculated based on the null hypothesis that the true parameters
#' are equal to zero. To test against other values a vector of "hypotheses" 
#' must be supplied. 
#' 
#' @param est A vector of model estimates
#' @param std_err A vector of standard errors
#' @param df An integer giving the degrees of freedom for the test
#' @param hypotheses A vector of values to test against. The vector must be the
#' same length as `est`. If no vector is supplied, the default test is against
#' 0. 
#' 
#' @return A vector of p-values the length of `est`
#' 
#' @export
pval <- function(est, std_err, df, hypotheses = NULL) {
  if (is.null(hypotheses)) {
    hypotheses <- rep(0, length(est))
    
  } else {
    stopifnot(length(est) == length(hypotheses))
    
  }
  
  return(
    2 * stats::pt(-abs((est - hypotheses) / std_err), df = df)
  )
}

#' Create significance stars
#' 
#' A convenient function that creates stars based on any vector of supplied 
#' p-values. 
#' 
#' @param pval A vector of p-values
#' 
#' @return A character vector of stars indicating significance at different 
#' levels
stars <- function(pval) {
  star <- rep("", length(pval))
  star[pval < 0.1] <- "."
  star[pval < 0.05] <- "*"
  star[pval < 0.01] <- "**"
  star[pval < 0.001] <- "***"
  
  return(
    star
  )
}

#' Calculate the convergence criteria
#' 
#' @inheritParams vcov.cmdlr
#'
#' @return A named numeric 
#' 
#' @export
convergence_criteria <- function(object, robust = TRUE, ...) {
  grad <- gradient(object)
  
  crit <- t(grad) %*% vcov(object, robust = robust) %*% grad
  
  attributes(crit) <- list(
    dim = NULL,
    names = "convergence_criteria"
  )
  
  return(
    crit
  )
}
