#' Prepares the draws
#' 
#' In a model using simulation, we need to prepare a set of random draws to 
#' use when simulating/approximating the distributions/integrals. The function
#' is a convenient wrapper around the function \code{\link{make_random_draws}}.
#' Depending on the model options, the function will transform the supplied
#' uniform draws to either standard uniform, standard normal or standard
#' triangular. It is up to the user to specify the correct distributions in the
#' log-likelihood function.
#' 
#' If we are estimating the model using multiple cores, then the function splits
#' the draws correctly based on the number of rows in \code{db}. Preparing the
#' draws for parallel processing without first having prepared the data, will 
#' cause the estimation to fail. This is because splitting the draws relies on 
#' the dimensions of the split data to ensure that the correct draws are assigned
#' to the correct individual.
#' 
#' The function is meant for internal use only.
#' 
#' @inheritParams estimate
#' 
#' @return A list of draws where each list element correspond to one dimension
#' or if we are using multiple cores, a list of lists where the top-level list
#' corresponds to the cores and the lower-level list corresponds to the draws. 

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
  message(green$bold(symbol$tick), "  Draws")
  draws
}
