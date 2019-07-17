#' Shuffle the order of points in the unit interval.
#' 
#' @param x A vector

shuffle <- function (x) {
  x[rank(runif(length(x)))]
}

#' Make Modified Latin Hypercube Draws
#' 
#' @inheritParams make_random_draws
#' 
#' @references 
#' Hess, S., Train, K. E. & Polak, J. W., 2006, On the use of a Modified Latin
#' Hypercube Sampling (MLHS) method in the estimation of a Mixed Logit Model
#' for vehicle choice, Transportation Research Part B, 40, pp. 147-163

make_mlhs <- function (N, R, D) {
  #   Define local parameters
  n <- N * R
  j <- 1L
  k <- 1L
  
  draws <- matrix(0, n, D)
  uniform <- seq(0, N - 1) / N
  
  while (j < R + 1L) {
    k <- 1L
    while (k < D + 1L) {
      draws[(1L + N * (j - 1L)):(N * j), k] <- shuffle(uniform + runif(1) / N)
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
#' @inheritParams make_random_draws
#' @param P A vector of prime numbers
#' @param count A matrix
#' @param digit A vector
#' 
#' @references Bhat, C. R., 2003, Simulation Estimation of Mixed Discrete Choice
#' Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855

digitize <- function (D, P, count, digit) {
  m <- 1L
  x <- NULL
  
  while (m <= D) {
    l <- 1L
    r <- 1
    while (r == 1) {
      x <- count[m, l] != (P[m] - 1)
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
#' @param perms A matrix of the permutations. Defaults to a set of Braaten-Weller
#' permutations. 
#' 
#' @references Bhat, C. R., 2003, Simulation Estimation of Mixed Descrete Choice
#' Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855

radical_inverse <- function (D, P, count, digit, perms) {
  m <- 1L
  G <- matrix(0, 1L, D)
  
  while (m <= D) {
    l <- 1L
    p <- P[m]
    while (l <= digit[m]) {
      G[m] <- (perms[m, (count[m, l] + 1L)] / p) + G[m]
      p <- p * P[m]
      l <- l + 1L
    }
    m <- m + 1L
  }
  return(G)
}

#' Make scrambled Halton draws
#' 
#' A function for creating scrambled Halton draws. The code is a translation of
#' the [GAUSS](http://www.caee.utexas.edu/prof/bhat/FULL_CODES.htm) codes written
#' by Professor Chandra Bhat. Note that the maximum number of dimensions for the
#' scrambled Halton draws is limited to 16. This is because only permutations up
#' to prime 16 are included in the permutation matrix. Extending to more than 
#' 16 dimensions can be achieved by including a different permutation matrix. 
#' 
#' The permutations are based on the Braaten-Weller algorithm. 
#' 
#' @inheritParams make_random_draws
#' 
#' @references Bhat, C. R., 2003, Simulation Estimation of Mixed Descrete Choice
#' Models Using Randomized and Scrambled Halton Sequences, Transportation
#' Research Part B, 9, pp. 837-855

make_scrambled_halton <- function (N, R, D) {
  if (D > 16) {
    stop("Cannot scramble sequences beyond the 16th prime")
  }
  
  max_digit <- 50
  
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53)[1L:D]
  H <- matrix(0, (N * R), D)
  
  #   Initialize the scrambling sequence
  count <- matrix(0, D, max_digit)
  count[, 1L] <- rep(1, D)
  digit <- rep(1, D)
  H[1L, ] <- radical_inverse(D, primes, count, digit, xbrat)
  
  j <- 2L
  while (j <= (N * R)) {
    count_digit <- digitize(D, primes, count, digit)
    count <- count_digit[["count"]]
    digit <- count_digit[["digit"]]
    H[j, ] <- radical_inverse(D, primes, count, digit, xbrat)
    j <- j + 1L
  }
  return(H)
}

#' Wrapper for halton()
#' 
#' Wrapper function for halton() from randtoolbox to create a common interface
#' 
#' @inheritParams make_random_draws

make_standard_halton <- function (N, R, D) {
  randtoolbox::halton(N * R, D)
}

#' Make sobol draws
#' 
#' Wrapper function for sobol() from randtoolbox to create a common interface
#' 
#' @inheritParams make_random_draws

make_standard_sobol <- function (N, R, D) {
  randtoolbox::sobol(N * R, D, scrambling = 0)
}

#' Make scrambled sobol draws
#' 
#' Wrapper function for sobol() from randtoolbox to create a common interface.
#' Owen + Fazure_Tezuka Scrambling
#' 
#' @inheritParams make_random_draws

make_scrambled_sobol <- function (N, R, D) {
  randtoolbox::sobol(N * R, D, scrambling = 3)
}

#' Make pseudo random draws
#' 
#' Wrapper for runif to create a common interface
#' 
#' @inheritParams make_random_draws

make_pseudo_random <- function (N, R, D) {
  matrix(runif(N * R * D), nrow = N * R, ncol = D)
}

#' Make random draws 
#' 
#' A common interface to creating a variety of random draws used to simulate
#' the log likelihood function
#' 
#' @param N Number of respondents in your sample
#' @param R Number of draws per respondent
#' @param D Number of dimensions 
#' @param type A character string
#'
#' @examples 
#' N <- 10
#' R <- 5
#' D <- 3
#' 
#' draws <- make_random_draws(N, R, D, "scrambled_sobol")
#' head(draws)
#' 
#' draws <- make_random_draws(N, R, D, "scrambled_halton")
#' head(draws)
#'
#'@export

make_random_draws <- function (N, R, D, type) {
  #   Catch accidental capitalization of the type of draws
  type_draw <- tolower(type)
  
  allowed_types <- c("pseudo_random", "mlhs", "standard_halton",
    "scrambled_halton", "standard_sobol", "scrambled_sobol")
  
  if (!(type_draw %in% allowed_types)) {
    stop("Unknown type of draws specified.") 
  }
  
  #   Set up a list of functions
  function_list <- list(make_pseudo_random, make_mlhs, make_standard_halton,
    make_scrambled_halton, make_standard_sobol,
    make_scrambled_sobol)
  names(function_list) <- allowed_types
  
  function_list[[type_draw]](N, R, D)
}
