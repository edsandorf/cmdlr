#' Attaches the objects passed through the list
#' 
#' This function attaches all objects passed through the list \code{objects}.
#' Each list element is an object to be attached.
#' 
#' @param objects An un-named list of objects to be attached
#' 
#' @return Nothing
#' 
#' @export

attach_objects <- function (objects) {
  lapply(objects, function(x) {
    if (is.data.frame(x)) attach(x)
    if (is.vector(x)) attach(as.list(x))
  })
}
