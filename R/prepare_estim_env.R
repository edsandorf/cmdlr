#' Prepare the estimation environment
#' 
#' To allow variables and parameters to be called by name, we need to set up 
#' an estimation environment to provide the context in which we evaluate the 
#' log-likelihood expression. 
#' 
#' This function is intended for internal use only. 
#' 
#' @inheritParams estimate
#' @inheritParams prepare_workers
#' 
#' @return An environment containing the data, draws and relevant indices to use
#' in estimation.
prepare_estim_env <- function(db, draws, model_opt) {
  estim_env <- rlang::env(
    # param_fixed = unlist(model_opt$param[model_opt$fixed]),
    N = length(unique(db[[model_opt[["id"]]]])),
    S = length(unique(db[[model_opt[["ct"]]]])),
    J = model_opt$J,
    choice_var = db[[model_opt$choice]]
  )
  
  # Add the data to the estimation environment
  invisible(
    lapply(seq_along(db), function(i) {
      assign(names(db[i]), db[[i]], envir = estim_env)
    })
  )
  
  # Add the draws to the estimation environment
  if (model_opt$mixing) {
    invisible(
      lapply(seq_along(draws), function(i) {
        assign(names(draws[i]), draws[[i]], envir = estim_env)
      })
    )
  }

  # Return the estimation environment
  estim_env
}
