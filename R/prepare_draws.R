#' Prepares the draws
#' 
#' A function that will create and prepare the draws for estimation
#' 
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#' 

prepare_draws <- function(db, estim_opt, model_opt) {
  # Extract the relevant information from model_opt() ----
  N <- model_opt$N
  S <- model_opt$S
  R <- model_opt$R
  type <- model_opt$draws_type
  rpar <- model_opt$rpar
  cores <- estim_opt$cores
  D <- length(rpar)
  
  # Make the draws and repeat rows equal to S - N*S x R
  draws <- make_random_draws(N, R, D, type)
  
  # Convert the draws to normal, uniform, triangular ----
  for (i in seq_along(rpar)) {
    if (rpar[[i]] == "normal") {
      draws[, i] <- stats::qnorm(draws[, i])
    }
    
    if (rpar[[i]] == "triangular") {
      index <- as.integer(draws[, i] < 0.5)
      draws[, i] <- index * (sqrt(2 * draws[, i]) - 1) + (1 - index) * (1 - sqrt(2 * (1 - draws[, i])))
    }
  }
  
  # Split the draws matrix if we are using multiple cores ----
  if (cores > 1) {
    # Create an index to split the draws
    the_rows <- lapply(db, function(x) {
      (nrow(x) / S) * R
    })
    rows_index <- vector(mode = "list", length = cores)
    rows_index[[1]] <- seq_len(the_rows[[1]])
    for (i in 2:cores) {
      rows_index[[i]] <- max(rows_index[[i - 1]]) + seq_len(the_rows[[i]])
    }
    
    # Split the draws into a list of length cores
    draws <- lapply(rows_index, function(i) {
      draws_core <- draws[i, , drop = FALSE]
      draws_core <- lapply(as.list(as.data.frame(draws_core)), function(x) {
        matrix(rep(matrix(x, ncol = R, byrow = TRUE), each = S), ncol = R)
      })
      names(draws_core) <- names(rpar)
      draws_core
    })
    
  } else {
    draws <- lapply(as.list(as.data.frame(draws)), function(x) {
      matrix(rep(matrix(x, ncol = R, byrow = TRUE), each = S), ncol = R)
    })
    names(draws) <- names(rpar)
  }
  
  # Return the list of draws
  cat(green$bold(symbol$tick), "  Draws\n")
  draws
}
