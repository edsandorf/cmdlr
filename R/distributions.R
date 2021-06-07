#' Transform distribution
#'
#' @param mu A value for the mean of the distribution
#' @param sigma A value for the standard deviation of the distribution
#' @param eta A numeric standard uniform vector
#' @param type The type of distribution
#'
#' @return A vector with the transformed distribution given the parameters
transform_distribution <- function(mu, sigma, eta, type) {
  switch(
    type,
    normal = transform_normal(mu, sigma, eta),
    lognormal = transform_lognormal(mu, sigma, eta),
    uniform = transform_uniform(mu, sigma, eta),
    triangular = transform_triangular(mu, sigma, eta),
  )
}

#' Transform to the normal distribution
#'
#' @inheritParams transform_distribution
transform_normal <- function(mu, sigma, eta) {
  mu + sigma * qnorm(eta)
}

#' Transform to the lognormal distribution
#'
#' @inheritParams transform_distribution
transform_lognormal <- function(mu, sigma, eta) {
  exp(mu + sigma * qnorm(eta))
}

#' Transform to the uniform distribution
#'
#' @inheritParams transform_distribution
transform_uniform <- function(mu, sigma, eta) {
  mu + sigma * (2 * eta - 1)
}

#' Transform to the triangular distribution
#'
#' @inheritParams transform_distribution
transform_triangular <- function(mu, sigma, eta) {
  idx <- as.integer(eta < 0.5)
  eta <- idx * (sqrt(2 * eta) - 1) + (1 - idx) * (1 - sqrt(2 * (1 - eta)))
  mu + sigma * eta
}
