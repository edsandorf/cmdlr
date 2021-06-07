#' Make random draws
#'
#' A common interface to creating a variety of random draws used to simulate
#' the log likelihood function
#'
#' @param n_id Number of individuals in your sample
#' @param n_draws Number of draws per respondent
#' @param n_dim Number of dimensions
#' @param seed A seed to change the scrambling of the sobol sequence.
#' @param type A character string indicating the type of draw:
#' "pseudo-random", "mlhs", "standard-halton", "scrambled-halton",
#' "standard-sobol", "scrambled-sobol"
#'
#' @return A matrix of dimensions n_id*n_draws x n_dim of standard uniform
#' draws
#'
#' @examples
#' n_id <- 10
#' n_draws <- 5
#' n_dim <- 3
#'
#' draws <- make_draws(n_id, n_draws, n_dim, seed = 10, "scrambled-sobol")
#' head(draws)
#'
#' draws <- make_draws(n_id, n_draws, n_dim, seed = 10, "scrambled-halton")
#' head(draws)
#'
#'@export
make_draws <- function(n_id, n_draws, n_dim, seed, type) {
  draws <- switch(
    type,
    `pseudo-random` = make_pseudo_random(n_id, n_draws, n_dim),
    `mlhs` = make_mlhs(n_id, n_draws, n_dim),
    `standard-halton` = make_standard_halton(n_id, n_draws, n_dim),
    `scrambled-halton` = make_scrambled_halton(n_id, n_draws, n_dim),
    `standard-sobol` = make_standard_sobol(n_id, n_draws, n_dim, seed),
    `scrambled-sobol` = make_scrambled_sobol(n_id, n_draws, n_dim, seed)
  )
  
  # Return as matrix
  if (is.matrix(draws)) {
    draws
  } else {
    as.matrix(draws)
  }
}

#' Shuffle the order of points in the unit interval.
#'
#' @param x A vector
shuffle <- function(x) {
  x[rank(runif(length(x)))]
}

#' Make Modified Latin Hypercube Draws
#'
#' @inheritParams make_draws
#'
#' @references
#' Hess, S., Train, K. E. & Polak, J. W., 2006, On the use of a Modified Latin
#' Hypercube Sampling (MLHS) method in the estimation of a Mixed Logit Model
#' for vehicle choice, Transportation Research Part B, 40, pp. 147-163
make_mlhs <- function(n_id, n_draws, n_dim) {
  #   Define local parameters
  n <- n_id * n_draws
  j <- 1L
  k <- 1L
  
  draws <- matrix(0, n, n_dim)
  uniform <- seq(0, n_id - 1) / n_id
  
  while (j < n_draws + 1L) {
    k <- 1L
    while (k < n_dim + 1L) {
      draws[(1L + n_id * (j - 1L)):(n_id * j), k] <- shuffle(
        uniform + runif(1) / n_id
      )
      k <- k + 1L
    }
    j <- j + 1L
  }
  return(draws)
}

#' Expand the sequence of integers
#'
#' Equation 1 in Bhat (2003)
#'
#' @inheritParams make_draws
#' @param primes A vector of prime numbers
#' @param count A matrix
#' @param digit A vector
#'
#' @references Bhat, C. n_draws., 2003, Simulation Estimation of Mixed Discrete
#' Choice Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855
digitize <- function(n_dim, primes, count, digit) {
  m <- 1L
  x <- NULL
  
  while (m <= n_dim) {
    l <- 1L
    r <- 1
    while (r == 1) {
      x <- count[m, l] != (primes[m] - 1)
      r <- r - x
      count[m, l] <- (count[m, l] + 1) * (x == 1)
      digit[m] <- ((l - 1L) == digit[m]) + digit[m]
      l <- l + 1L
    }
    m <- m + 1L
  }
  return(list(count = count,
              digit = digit))
}

#' Compute the radical inverse
#'
#' Equation 2 in Bhat (2003)
#'
#' @inheritParams digitize
#' @param perms A matrix of the permutations. Defaults to a set of
#' Braaten-Weller permutations.
#'
#' @references Bhat, C. n_draws., 2003, Simulation Estimation of Mixed Descrete
#' Choice Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855
radical_inverse <- function(n_dim, primes, count, digit, perms) {
  m <- 1L
  g <- matrix(0, 1L, n_dim)
  
  while (m <= n_dim) {
    l <- 1L
    p <- primes[m]
    while (l <= digit[m]) {
      g[m] <- (perms[m, (count[m, l] + 1L)] / p) + g[m]
      p <- p * primes[m]
      l <- l + 1L
    }
    m <- m + 1L
  }
  return(g)
}

#' Make scrambled Halton draws
#'
#' A function for creating scrambled Halton draws. The code is a translation of
#' the [GAUSS](http://www.caee.utexas.edu/prof/bhat/FULL_CODES.htm) codes
#' written by Professor Chandra Bhat. Note that the maximum number of dimensions
#' for the scrambled Halton draws is limited to 16. This is because only
#' permutations up to prime 16 are included in the permutation matrix. Extending
#' to more than 16 dimensions can be achieved by including a different
#' permutation matrix.
#'
#' The permutations are based on the Braaten-Weller algorithm.
#'
#' @inheritParams make_draws
#'
#' @references Bhat, C. n_draws., 2003, Simulation Estimation of Mixed Descrete
#' Choice Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855
make_scrambled_halton <- function(n_id, n_draws, n_dim) {
  if (n_dim > 16) {
    stop("Cannot scramble sequences beyond the 16th prime")
  }
  
  max_digit <- 50
  
  primes <- c(
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53
  )[1L:n_dim]
  h <- matrix(0, (n_id * n_draws), n_dim)
  
  #   Initialize the scrambling sequence
  count <- matrix(0, n_dim, max_digit)
  count[, 1L] <- rep(1, n_dim)
  digit <- rep(1, n_dim)
  h[1L, ] <- radical_inverse(n_dim, primes, count, digit, xbrat)
  
  j <- 2L
  while (j <= (n_id * n_draws)) {
    count_digit <- digitize(n_dim, primes, count, digit)
    count <- count_digit[["count"]]
    digit <- count_digit[["digit"]]
    h[j, ] <- radical_inverse(n_dim, primes, count, digit, xbrat)
    j <- j + 1L
  }
  return(h)
}

#' Wrapper for halton()
#'
#' Wrapper function for halton() from randtoolbox to create a common interface
#'
#' @inheritParams make_draws
make_standard_halton <- function(n_id, n_draws, n_dim) {
  randtoolbox::halton(n_id * n_draws, n_dim)
}

#' Make sobol draws
#'
#' Wrapper function for sobol() from randtoolbox to create a common interface
#'
#' @inheritParams make_draws
make_standard_sobol <- function(n_id, n_draws, n_dim, seed = seed) {
  randtoolbox::sobol(n_id * n_draws, n_dim, seed = seed, scrambling = 0)
}

#' Make scrambled sobol draws
#'
#' Wrapper function for sobol() from randtoolbox to create a common interface.
#' Owen + Fazure_Tezuka Scrambling
#'
#' @inheritParams make_draws
make_scrambled_sobol <- function(n_id, n_draws, n_dim, seed = seed) {
  randtoolbox::sobol(n_id * n_draws, n_dim, seed = seed, scrambling = 3)
}

#' Make pseudo random draws
#'
#' Wrapper for runif to create a common interface
#'
#' @inheritParams make_draws
make_pseudo_random <- function(n_id, n_draws, n_dim) {
  matrix(runif(n_id * n_draws * n_dim), nrow = n_id * n_draws, ncol = n_dim)
}

