#' Compare choice shares
#' 
#' The function calculates the average value of the explanator for each
#' alternative when chosen and not chosen.
#'
#' @inheritParams identify_choice_patterns
#' @param alt_avail A list the length of the total number of alternatives. Each
#' list element is a vector of length equal to the number of observations with
#' the values 0 and 1 indicating unvailable/available aleternatives in each 
#' choice situation 
#' @param explanators A character vector with the names of the variables for
#' which to calcualte differences in choice shares
#' 
#' The function is a rewritten version of \code{apollo_choiceAnalysis()}. 
#'
#' @references 
#' Hess, S. & Palma, D. (2019), Apollo: a flexible, powerful and customisable 
#' freeware package for choice model estimation and application, Journal of 
#' Choice Modelling, Volume 32, September 2019, 100170
#' Hess, S. & Palma, D. (2019), Apollo version 0.1.0, user manual,
#'  www.ApolloChoiceModelling.com
#'
#' @return A list with the choice shares and test statistics for difference in
#' means
#'  
#' @export
compare_choice_shares <- function(db, str_choice, alt_avail, explanators = NULL) {
  # Extract useful information
  n_obs <- nrow(db)
  choices <- db[[str_choice]]
  choices_tbl <- table(choices)
  
  # Calculate overall choice shares
  choice_shares <- list(
    matrix(c(choices_tbl,
             choices_tbl/n_obs,
             rep(n_obs, length(alt_avail))), ncol = 3,
           dimnames = list(paste0("Alternative ", seq_along(alt_avail)),
                           c("Choices", "Share", "Total")))
  )
  
  # Calculate the choice shares by sub-group for each alternative
  if (!is.null(explanators)) {
    choice_shares_lst <- lapply(explanators, function(x, choices) {
      explanator_shares <- lapply(seq_along(alt_avail), function(j, x, choices) {
        available <- alt_avail[[j]] == 1
        chosen <- choices[available] == j
        
        # Extract the explanator and calculate the mean by whether or not it was chosen
        explanator <- db[[x]][available]
        explanator_shares <- tapply(explanator, chosen, mean, na.rm = TRUE)
        explanator_shares <- cbind(
          t(as.matrix(explanator_shares)),
          stats::t.test(explanator[!chosen], explanator[chosen])[["statistic"]]
        )
        
        return(explanator_shares)
      }, x, choices)
      
      # Combine, reorder and set names
      explanator_shares <- do.call(rbind, explanator_shares)[, c(2, 1, 3)]
      colnames(explanator_shares) <- c(paste0("Mean ", x, " if chosen"), 
                                       paste0("Mean ", x, " if not chosen"),
                                       "T-test statistic")
      rownames(explanator_shares) <- paste0("Alternative ",
                                            seq_len(nrow(explanator_shares)))
      
      return(explanator_shares)
      
    }, choices)
    
    choice_shares <- c(choice_shares, choice_shares_lst)
    names(choice_shares) <- c("overall", explanators)
  }
  
  class(choice_shares) <- "choice_shares"
  return(choice_shares)
}

#' Identify choice patterns
#'
#' @param db A data.frame or tibble
#' @param str_id A character string giving the name of the individual id 
#' variable
#' @param str_ct A character string giving the name of the choice task variable
#' @param str_choice A character string giving the name of the choice variable
#' @param sq_alt A number giving the alternative (in the choice variable) 
#' corresponding to the status quo or opt out
#' 
#' @return A vector the length of rows in `db` with value equal to 1 if an 
#' individual always chose the SQ or opt out alternative
#' 
#' @export
identify_choice_patterns <- function(db, str_id, str_ct, str_choice, sq_alt) {
  choice_patterns <- lapply(unique(db[[str_id]]), function(x) {
    idx <- db[[str_id]] == x
    choices <- db[[str_choice]][idx]
    alts <- sort(unique(db[[str_choice]]))
    rows <- length(choices)
    
    return(
      tibble::tibble(always_sq = rep(1L * all(choices == sq_alt), rows),
                     never_sq = rep(1L * all(choices != sq_alt), rows),
                     sometimes_sq = 1L - (always_sq + never_sq))
    )
  })
  
  choice_patterns <- do.call(dplyr::bind_rows, choice_patterns)
  return(choice_patterns)
}

#' Expand choice variable
#' 
#' Expands the vector of choice variables to a tibble with dummy columns for
#' whether an alternative was chosen. The function uses the sorted unique
#' values of the choice variable to infer the total number of alternatives
#' available (irrespective of whether any of them were chosen)
#' 
#' @inheritParams identify_choice_patterns
#' 
#' @return A tibble with the expanded choice variable
#' 
#' @export
expand_choices <- function(db, str_id, str_ct, str_choice) {
  choice_vars <- lapply(unique(db[[str_id]]), function(x) {
    idx <- db[[str_id]] == x
    choices <- db[[str_choice]][idx]
    alts <- sort(unique(db[[str_choice]]))
    rows <- length(choices)
    
    return(
      eval(
        parse(
          text = paste0(
            "tibble::tibble(", 
            paste(
              paste0(
                "choice_",
                alts,
                " = ifelse(choices == ",
                alts,
                ", 1, 0)"
              ),
              collapse = ","
            ),
            ")"
          )
        )
      )
    )
  })
  
  return(do.call(bind_rows, choice_vars))
}

#' Plot choice shares
#' 
#' @inheritParams identify_choice_patterns
#' 
#' @return A plot
#' 
#' @export
plot_choice <- function(db,
                        str_id,
                        str_ct,
                        str_choice) {
  
  bind_cols(
    db %>% 
      dplyr::select(str_id, str_ct),
    expand_choices(db, str_id, str_ct, str_choice)
  ) %>%
    tidyr::pivot_longer(starts_with("choice_"), names_to = "alt", values_to = "choice") %>%
    dplyr::mutate(
      !!str_ct := as_factor(!!sym(str_ct)),
      alt = as_factor(alt)
    ) %>%
    dplyr::filter(choice == 1) %>%
    group_by(!!sym(str_ct), alt) %>% # Create an if to handle block
    dplyr::summarize(
      n_choices = n()
    ) %>%
    ggplot() + 
    geom_bar(stat = "identity", position = "fill", aes(x = !!sym(str_ct), y = n_choices, fill = alt)) + 
    ggtitle("Distribution of choice shares by alternative across choice tasks") +
    xlab("Choice task") +
    ylab("Share of choices") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = paste0(seq(0, 100, by = 10), "%")) + 
    labs(fill = "Chosen alternative")
}
