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
  
  ct_tmp <- data.frame(X = seq_len(S))
  
  names(ct_tmp) <- model_opt[["ct"]]
  
  db <- lapply(unique(db[[model_opt[["id"]]]]), function(i) {
    db_tmp <- db[db[, model_opt[["id"]]] == i, ]
    if (nrow(db_tmp) != S) {
      db_tmp <- merge(ct_tmp, db_tmp, by = model_opt[["ct"]], all.x = TRUE)
      db_tmp$id <- i
    }
    
    # Return the padded data matrix
    db_tmp
  })
  
  db <- do.call(rbind, db)
  db
}
