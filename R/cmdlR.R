#' cmdlR: Choice Modeling in R
#' 
#' The problem of choice is fundamental to economics and understanding how 
#' people make decisions is important. As researchers we try to find or 
#' generate data through experimetns that will enable us to understand bbetter
#' how people make decisions. Choice modeling is the art of applying 
#' choice models to data. 
#' 
#' This R package contains an easy to use framework that will help an
#' econometrician estimate choice models in R. It is the culmination of years 
#' of working with choice models and programming in R. It is the framework
#' that I use in my research when I estimate such models. 
#' 
#' I make no claims that this package is better than others and indeed 
#' packages such as Apollo includes much more built in functionality and much
#' more help to the user.
#' 
#' @docType package
#' @name cmldr
#' 
#' @importFrom stats runif qnorm
#' @importFrom rlang sym :=
#' @importFrom dplyr bind_cols bind_rows n group_by starts_with
#' @importFrom magrittr %>%
#' @importFrom forcats as_factor
#' @importFrom ggplot2 ggplot geom_bar aes ggtitle xlab ylab scale_y_continuous labs
NULL
