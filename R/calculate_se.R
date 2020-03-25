#' Calculate the standard error of the variance
#' 
#' Calculate the standard error of the variance terms in the variance-covariance
#' matrix created from the lower Cholesky decomposition matrix of the random 
#' parameters in a mixed logit model. 
#' 
#' @param a An integer indicating the row position of the random parameter in the
#' asymptotic variance-covariance matrix
#' @param L The lower cholesky matrix 
#' @param lambda The asymptotic variance covariance matrix
#' 
#' @references 
#' http://www.stephanehess.me.uk/software.html
#' Daly, A. J., Hess, S., & de Jong, G., 2012, Calculating errors for measures derived
#' from choice modelling estimates, Transportation Research Part B, 46(2):333-341
#' 
#' @export
fn_se_var <- function(a, L, lambda){
  sqrt(t(fn_1dv(a, L)) %*% lambda %*% fn_1dv(a, L))
}

#' Calculate the standard error of the covariance
#' 
#' Calculate the standard errors of the covariance terms in the variance-covariance
#' matrix created from the lower Cholesky decomposition matrix of the random 
#' parameters in a mixed logit model. 
#' 
#' @param a An integer indicating the row position of the random parameter in the
#' asymptotic variance-covariance matrix
#' @param b An integer indicating the column position of the random parameter in the
#' asymptotic variance-covariance matrix
#' @param L The lower cholesky matrix 
#' @param lambda The asymptotic variance covariance matrix
#' 
#' @references 
#' http://www.stephanehess.me.uk/software.html
#' Daly, A. J., Hess, S., & de Jong, G., 2012, Calculating errors for measures derived
#' from choice modelling estimates, Transportation Research Part B, 46(2):333-341
#' 
#' @export
fn_se_cov <- function(a, b, L, lambda){
  sqrt(t(fn_2dv(a, b, L)) %*% lambda %*% fn_2dv(a, b, L))
}

#' Calculate the standard error of the correlation
#' 
#' Calculate the standard error of the correlations between the elments of the
#' variance covariance matrix created from the lower Cholesky decomposition matrix of the random 
#' parameters in a mixed logit model. 
#' 
#' @inheritParams fn_se_cov
#' 
#' @references 
#' http://www.stephanehess.me.uk/software.html
#' Daly, A. J., Hess, S., & de Jong, G., 2012, Calculating errors for measures derived
#' from choice modelling estimates, Transportation Research Part B, 46(2):333-341
#' 
#' @export
fn_se_corr <- function(a, b, L, lambda){
  sqrt(t(fn_3dv(a, b, L)) %*% lambda %*% fn_3dv(a, b, L))
}

#' Calcualte the standard error of the standard deviation
#' 
#' The standard deviation is calculated as the square root of the diagonal of 
#' the variance coavariance matrix of your random parameters. The variance-
#' covariance matrix is calculated using the estimated lower Cholesky matrix.
#' 
#' @inheritParams fn_se_var
#' 
#' @return  A vector of standard errors

#' 
#' @references 
#' http://www.stephanehess.me.uk/software.html
#' Daly, A. J., Hess, S., & de Jong, G., 2012, Calculating errors for measures derived
#' from choice modelling estimates, Transportation Research Part B, 46(2):333-341
#' 
#' @export
fn_se_std <- function(a, L, lambda){
  sqrt(t(fn_4dv(a, L)) %*% lambda %*% fn_4dv(a, L))
}


#' Helper function fn_1
#' 
#' Helper function to calculate the standard errors of standard deviations and 
#' correlations from a MIXL model. Together, all the helper functions and the 
#' four functions \code{\link{fn_se_var}}, \code{\link{fn_se_cov}}, 
#' \code{\link{fn_se_corr}} and \code{\link{fn_se_std}}, is an application 
#' of the Delta method. The helper functions are for internal use only and are 
#' not exported.
#' 
#' @inheritParams fn_se_cov
#' 
#' @references 
#' http://www.stephanehess.me.uk/software.html
#' Daly, A. J., Hess, S., & de Jong, G., 2012, Calculating errors for measures derived
#' from choice modelling estimates, Transportation Research Part B, 46(2):333-341
fn_1 <- function(a, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  l <- 1
  out <- 0
  while(l < (a + 1)){
    out <- out + L[a, l]^2
    l <- l + 1
  }
  
  return(out)
}

#' Helper function fn_1d
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
#' @param j Indexing parameter
#' @param k Indexing parameter 
fn_1d <- function(a, j, k, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  if(k > j){
    stop(k, ">", j, ": k needs to be smaller than or equal to k.")
  }
  
  out <- 0
  if(a == j){
    out <- 2 * L[a, k]
  }
  return(out)
}

#' Helper function fn_1dv
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_1dv <- function(a, L){
  K <- sum(1:nrow(L))
  out <- matrix(0, K, 1)
  
  j <- 1
  n <- 1
  
  while(j < (nrow(L) + 1)){
    k <- 1
    while(k < (j + 1)){
      out[n, 1] <- fn_1d(a, j, k, L)
      k <- k + 1
      n <- n + 1
    }
    j <- j + 1
  }
  return(out)
}

#' Helper function fn_2
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_2 <- function(a, b, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  if(b > nrow(L)){
    stop(b, " exceeds number of coefficients.")
  }
  
  if(a > b){
    stop(a, ">", b, ": reverse order of coefficients.")
  }
  
  l <- 1
  out <- 0
  
  while(l < (a + 1)){
    out <- out + L[a, l] * L[b, l]
    l <- l + 1
  }
  return(out)
}

#' Helper function fn_2d
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
#' @param j Indexing parameter
#' @param k Indexing parameter 
fn_2d <- function(a, b, j, k, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  if(b > nrow(L)){
    stop(b, " exceeds number of coefficients.")
  }
  
  if(a > b){
    stop(a, ">", b, ": reverse order of coefficients.")
  }
  
  if(a == b){
    stop(a, "=", b, ": use function with two separate coefficients.")
  }
  
  if(k > j){
    stop(k, ">", j, ": k needs to be smaller than or equal to k.")
  }
  
  l <- 1
  out <- 0
  
  if(a == j){
    out <- L[b, k]
  }
  
  if(b == j){
    out <- L[a, k]
  }
  
  return(out)
}

#' Helper function fn_2dv
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_2dv <- function(a, b, L){
  K <- sum(1:nrow(L))
  out <- matrix(0, K, 1)
  j <- 1
  n <- 1
  
  while(j < (nrow(L) + 1)){
    k <- 1
    while(k < (j + 1)){
      out[n, 1] <- fn_2d(a, b, j, k, L)
      k <- k + 1
      n <- n + 1
    }
    j <- j + 1
  }
  return(out)
}

#' Helper function fn_3
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_3 <- function(a, b, L){
  out <- fn_2(a, b, L) / (sqrt(fn_1(a, L) * fn_1(b, L)))
  return(out)
}

#' Helper function fn_3d
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
#' @param j Indexing parameter
#' @param k Indexing parameter 
fn_3d <- function(a, b, j, k, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  if(b > nrow(L)){
    stop(b, " exceeds number of coefficients.")
  }
  
  if(a > b){
    stop(a, ">", b, ": reverse order of coefficients.")
  }
  
  if(a == b){
    stop(a, "=", b, ": use function with two separate coefficients.")
  }
  
  if(k > j){
    stop(k, ">", j, ": k needs to be smaller than or equal to k.")
  }
  
  l <- 1
  
  out <- ((fn_2d(a, b, j, k, L) * sqrt(fn_1(a, L) * fn_1(b, L)) - fn_2(a, b, L)) /
            (2 * (sqrt(fn_1(a, L) * fn_1 (b, L)))) * 
            (fn_1d(a, j, k, L) * fn_1(b, L) + fn_1d(b, j, k, L) * fn_1(a, L)))/
    (fn_1(a, L) * fn_1(b, L))
  
  return(out)
}

#' Helper function fn_3dv
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_3dv <- function(a, b, L){
  K <- sum(1:nrow(L))
  out <- matrix(0, K, 1)
  j <- 1
  n <- 1
  
  while(j < (nrow(L) + 1)){
    k <- 1
    while(k < (j + 1)){
      out[n, 1] <- fn_3d(a, b, j, k, L)
      k <- k + 1
      n <- n + 1
    }
    j <- j + 1
  }
  return(out)
}

#' Helper function fn_4
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_4 <- function(a, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  l <- 1
  out <- 0
  while(l < (a + 1)){
    out <- out + L[a, l]^2
    l = l + 1
  }
  out <- sqrt(out)
  return(out)
}

#' Helper function fn_4d
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
#' @param j Indexing parameter
#' @param k Indexing parameter 
fn_4d <- function(a, j, k, L){
  if(a > nrow(L)){
    stop(a, " exceeds number of coefficients.")
  }
  
  if(k > j){
    stop(k, ">", j, ": k needs to be smaller than or equal to k.")
  }
  
  out <- 0
  if(a == j){
    out = 1 /(2 * fn_4(a, L)) * 2 * L[a, k]
  }
  return(out)
}

#' Helper function fn_4dv
#' 
#' For a description see \code{\link{fn_1}}
#' 
#' @inheritParams fn_se_cov
fn_4dv <- function(a, L){
  K <- sum(1:nrow(L))
  out = matrix(0, K, 1)
  j <- 1
  n <- 1
  
  while(j < (nrow(L) + 1)){
    k <- 1
    while(k < (j + 1)){
      out[n, 1] <- fn_4d(a, j, k, L)
      k <- k + 1
      n <- n + 1
    }
    j <- j + 1
  }
  return(out)
}
