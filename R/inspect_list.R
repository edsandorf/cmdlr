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
