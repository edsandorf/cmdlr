#' Repeat rows
#'
#' Repeats each row in the matrix or data frame 'x' a number of times equal to
#' 'times'.
#'
#' @param x A matrix or data frame
#' @param times An integer indicating the number of times to repeat the
#' row/column
#'
#' @examples
#' test_matrix <- matrix(runif(12), 4)
#' rep_rows(test_matrix, 2)
#'
#' @export
rep_rows <- function(x, times) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  
  return(x[rep(seq_len(nrow(x)), each = times), , drop = FALSE])
}

#' Repeat columns
#'
#' Repeats each column of the matrix or data frame 'x' a number of times equal
#' to 'times'.
#'
#' @inheritParams rep_rows
#'
#' @examples
#' test_matrix <- matrix(runif(12), 4)
#' rep_cols(test_matrix, 2)
#'
#' @export
rep_cols <- function(x, times) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  
  return(x[, rep(seq_len(ncol(x)), each = times), drop = FALSE])
}

#' Inspect a list
#' 
#' A useful function for exploratory work or when coding. Takes a list as an
#' argument and prints a glimpse of the data contained in each list element 
#' and the dimensions of each list element. If the list contains more than 5
#' list elements, only the first 5 elements are printed to console with no 
#' warning. The function will print a maximum of 10 values if the list element
#' is a vector and 10 rows and 5 columns if the list element is a matrix. 
#' 
#' NOTE: There are currently no checks built in to handle a list of lists. In
#' this case, everything in the sublists will be printed with no checks or 
#' warnings.
#' 
#' @param x A list of vectors or matrices
#' 
#' @examples 
#' x <- list(
#'   matrix(runif(100), nrow = 20, ncol = 5),
#'   matrix(runif(100), nrow = 10, ncol = 10)
#' )
#' 
#' inspect_list(x)
#' 
#' @export
inspect_list <- function(x) {
  if (!is.list(x)) stop("Input must be a list")
  if (length(x) > 5) x <- x[seq_len(5)]
  lapply(x, function(y) {
    if (is.null(dim(y))) {
      if (length(y) < 10) {
        list(y, length(y))
      } else {
        list(y[seq_len(10)], length(y))
      }
    } else {
      if (nrow(y) > 10 & ncol(y) > 5) {
        list(y[seq_len(10), seq_len(5)], dim(y))
      } else if (nrow(y) > 10 & ncol(y) <= 5) {
        list(y[seq_len(10), ], dim(y))
      } else if (nrow(y) <= 10 & ncol(y) > 5) {
        list(y[, seq_len(5)], dim(y))
      } else {
        list(y, dim(y))
      }
    }
  })
}


#' Load a multiple packages
#' 
#' Loads all packages in 'pkgs'
#' 
#' @param pkgs A character string of packages
#' 
#' @return NULL
#' 
#' @export
load_packages <- function(pkgs) {
  stopifnot(is.character(pkgs))
  
  lapply(pkgs, require, character.only = TRUE)
  
  return(NULL)
}

#' Standardize a variable
#'
#' Standardize a variable by subtracting the mean and dividing by the standard
#' deviation
#' 
#' @param x A numeric vector
#' @param na.rm A boolean equal to TRUE if ignore NA when calculating the mean
#' and standard deviations. See \code{\link{mean}} and \code{\link{sd}}
#'
#' @examples
#' x <- runif(10)
#' standardize(x)
#' 
#' x[1] <- NA
#' standardize(x, na.rm = TRUE)
#' 
#' @return A vector of standardized numbers
#' 
#' @export
standardize <- function(x, na.rm = FALSE) {
  return(
    (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
  )
}
