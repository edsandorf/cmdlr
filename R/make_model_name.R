#' A function to create a model name
#'
#' @param model_name A string
#'

make_model_name <- function(model_name) {
  gsub("([.\\s+])+", "-", tolower(model_name), perl = TRUE)
}
