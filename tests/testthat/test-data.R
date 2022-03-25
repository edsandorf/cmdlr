context("Test data functions")

ll <- function(param) {
  V <- list(
    alt1 = asc_1 + b_1 * variable_1,
    alt2 = asc_2 + b_2 * var_2
  )
}

db <- tibble::tibble(
  variable_1 = runif(10),
  var_2 = runif(10),
  id = seq(1, 10, 1),
  ct = rep(1:2, 5),
  income = sample(100:1000, 10)
)


test_that("subset_data() correctly handles additional arguments", {
  expect_equal(
    subset_data(ll, db),
    db %>% dplyr::select(variable_1, var_2)
  )
  
  expect_equal(
    subset_data(ll, db, c("id", "ct")),
    subset_data(ll, db, "id", "ct")
  )
  
  expect_equal(
    subset_data(ll, db, c("id", "ct"), "income"),
    subset_data(ll, db, "id", "ct", "income")
  )
  
  expect_equal(
    subset_data(ll, db, c("id", "ct"), "income"),
    subset_data(ll, db, "id", "income", "ct")
  )
  
  expect_error(
    subset_data(ll, db, c("variable_1", var2))
  )
  
  expect_error(
    subset_data(ll, db, c("variable_1", var2), "var2")
  )
  
})
