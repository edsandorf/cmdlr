#' Repeat rows
#'
#' Repeats each row in the matrix or data frame 'x' a number of times equal to
#' 'times'.
#' 
#' @param x A matrix or data frame 
#' @param times An integer indicating the number of times to repeat the row/column
#' 
#' @examples 
#' test_matrix <- matrix(runif(12), 4)
#' rep_rows(test_matrix, 2)
#'
#' @export

rep_rows <- function(x, times) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("'x' must be a data.frame or matrix")
  }
  
  x[rep(seq_len(nrow(x)), each = times), ]
}

#' Repeat columns
#'
#' Repeats each column of the matrix or data frame 'x' a number of times equal to
#' 'times'.
#' 
#' @inheritParams rep_rows
#' 
#' @examples 
#' test_matrix <- matrix(runif(12), 4)
#' rep_cols(test_matrix, 2)
#' 
#' @export

rep_cols <- function(x, times) {
  if (!(is.data.frame(x) || is.matrix(x))) {
    stop("'x' must be a data.frame or matrix")
  }
  
  x[, rep(seq_len(ncol(x)), each = times)]
}
