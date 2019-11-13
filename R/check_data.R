#' Function to check the data
#'
#' The function will check the data to see if it is balanced. It will return
#' a list of conditions indicating which aspects of the data is unbalanced,
#' which enables the code to call the correct functions to set up the data.
#' 
#' @return A list of conditions
#' 
#' @param db Data
#' @param estim_opt List of estimation options
#' @param model_opt List of model options
#'
#' @export

check_data <- function(db, estim_opt, model_opt) {
  cat(black$bold("Checking data ...\n"))
  
  N <- length(unique(db[[model_opt[["id"]]]]))
  S <- length(unique(db[[model_opt[["ct"]]]]))

  # Check choice tasks
  if ((N * S) != nrow(db)) {
    cat(yellow$bold("Warning: " %+% reset$silver("Unequal number of choice occasions across individuals. Padding data with NA ... \n")))
    
    ct_tmp <- tibble(ct = seq_len(S))
    db <- lapply(deframe(unique(db[, "id"])), function(i) {
      db_tmp <- db[db[, "id"] == i, ]
      if (nrow(db_tmp) != S) {
        db_tmp <- left_join(ct_tmp, db_tmp, by = "ct") %>%
          mutate(id = i)
      }
      
      # Return the padded data matrix
      return(db_tmp)
    })
    
    db <- do.call(rbind, db)
    cat(green$bold("Success: " %+% reset$silver("Data padded successfully.\n")))
  }
  
  # Return the manipulated and checked data
  return(db)
}
