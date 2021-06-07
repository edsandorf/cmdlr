#' Analyzes choices
#' 
#' The function compares the market shares across sub-samples in the data. The 
#' user can specify which, if any, explanators to use in the choice analysis. 
#' The choice analysis does not involve any estimation. 
#' 
#' The function is a rewritten version of \code{apollo_choiceAnalysis()}. See
#' references. 
#' 
#' @param db A data.frame containing the estimation data
#' @inheritParams estimate
#' 
#' @return A matrix with the choice shares and test statistics for difference in
#' means
#' 
#' @examples 
#' \dontrun{
#'   model_opt <- list(
#'     choice_analysis_explanators = NULL
#'   )
#'   
#'   # OR
#'   
#'   model_opt <- list(
#'     choice_analysis_explanators = c("male", "income")
#'   )
#'   
#'   analyze_choices(db, model_opt)
#' }
#' 
#' @references 
#' Hess, S. & Palma, D. (2019), Apollo: a flexible, powerful and customisable 
#' freeware package for choice model estimation and application, Journal of 
#' Choice Modelling, Volume 32, September 2019, 100170
#' Hess, S. & Palma, D. (2019), Apollo version 0.1.0, user manual,
#'  www.ApolloChoiceModelling.com
#' 
#' @export

analyze_choices <- function(db, validated_options) {
  # Extract useful information
  model_opt <- validated_options[["model_opt"]]
  
  alt_avail <- model_opt$alt_avail
  choice_var <- db[[model_opt[["choice"]]]]
  
  # Calculate the choice shares by sub-group for each alternative
  choice_shares <- lapply(seq_along(alt_avail), function(j) {
    available <- alt_avail[[j]] == 1
    chosen <- choice_var[available] == j
    
    choice_sub_shares <- lapply(model_opt$choice_analysis_explanators, function(x) {
      # Extract the explanator and calculate the mean by whether or not it was chosen
      explanator <- db[[x]][available]
      tbl <- tapply(explanator, chosen, mean, na.rm = TRUE)
      
      # Define the output table
      output <- matrix(NA, nrow = 1, ncol = 3)
      colnames(output) <- c(paste0("Mean ", x, " if chosen"), 
                            paste0("Mean ", x, " if not chosen"),
                            "T-test statistic")
      rownames(output) <- paste0("Alternative ", j)
      
      output[, 1] <- tbl[2]
      output[, 2] <- tbl[1]
      if (tbl[1] != tbl[2]) {
        output[, 3] <- stats::t.test(explanator[!chosen], explanator[chosen])[["statistic"]]  
      }
      
      # Return the output
      output
    })
    
    # Return the choice sub shares
    Reduce(cbind, choice_sub_shares)
  })
  
  # Implicitly return the matrix of choice shares
  Reduce(rbind, choice_shares)
}
