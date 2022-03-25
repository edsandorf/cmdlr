context("Test assertions")

ll <- function(param) {
  V <- list(
    alt1 = asc_1 + b_1 * variable_1,
    alt2 = asc_2 + b_2 * var_2
  )
}


test_that("is_used correctly returns TRUE/FALSE if a variable is
          included in the log likelihood function", {
  expect_true(
    is_used("var_2", ll)
  )
  expect_true(
    is_used("asc_1", ll)
  )
  expect_false(
    is_used("variable", ll)
  )
  expect_false(
    is_used("var_", ll)
  )
  expect_true(
    is_used("variable_1", ll)
  )
  expect_true(
    is_used("alt1", ll)
  )
})
