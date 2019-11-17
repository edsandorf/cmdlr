#' Pads the data
#'
#' The function pads the data by adding rows with NA in case of missing choice
#' occasions. 
#'
#' @param db Data
#' @param model_opt List of model options
#' 
#' @return A tibble with the padded data. 

pad_data <- function(db, model_opt) {
  N <- length(unique(db[[model_opt[["id"]]]]))
  S <- length(unique(db[[model_opt[["ct"]]]]))
  
  ct_tmp <- tibble(X = seq_len(S))
  names(ct_tmp) <- model_opt[["ct"]]
  
  db <- lapply(deframe(unique(db[, model_opt[["id"]]])), function(i) {
    db_tmp <- db[db[, model_opt[["id"]]] == i, ]
    if (nrow(db_tmp) != S) {
      db_tmp <- left_join(ct_tmp, db_tmp, by = model_opt[["ct"]]) %>%
        mutate(id = i)
    }
    
    # Return the padded data matrix
    return(db_tmp)
  })
  
  db <- do.call(rbind, db)
  return(db)
}
