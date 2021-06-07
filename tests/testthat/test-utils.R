context("Test utils")

x <- matrix(1:4, nrow = 2)

test_that("Repeat rows correctly repeats the rows of a matrix", {
  expect_equal(
    rep_rows(x, 2),
    structure(
      c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
      .Dim = c(4L, 2L)
    )
  )
})

test_that("Repeat columns correctly repeats the columns of a matrix", {
  expect_equal(
    rep_cols(x, 2),
    structure(
      c(1L, 2L, 1L, 2L, 3L, 4L, 3L, 4L),
      .Dim = c(2L, 4L)
    )
  )
})
