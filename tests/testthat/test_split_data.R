context("Test that split_data() doesn't split the same individual on multiple
        cores.")

test_that("split_data() doesn't split the same individual on multiple cores", {
  db <- cmdlr::data_petr_test
  
  cores <- 3
  db_split <- split_data(db, "id", cores)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, "id"])
  }))))
  
  cores <- 5
  db_split <- split_data(db, "id", cores)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, "id"])
  }))))
  
  cores <- 10
  db_split <- split_data(db, "id", cores)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, "id"])
  }))))
  
})
