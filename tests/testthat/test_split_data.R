context("Test that split_data() doesn't split the same individual on multiple
        cores.")

test_that("split_data() doesn't split the same individual on multiple cores", {
  model_opt <- list(id = "id",
                    ct = "ct")
  estim_opt <- list(cores = 1)
  db <- cmdlR::data_petr_test
  db <- pad_data(db, model_opt)
  
  estim_opt <- list(cores = 3)
  db_split <- split_data(db, estim_opt, model_opt)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, model_opt$id])
  }))))
  
  estim_opt <- list(cores = 5)
  db_split <- split_data(db, estim_opt, model_opt)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, model_opt$id])
  }))))
  
  estim_opt <- list(cores = 10)
  db_split <- split_data(db, estim_opt, model_opt)
  expect_true(all(!Reduce("%in%", lapply(db_split, function(x) {
    unique(x[, model_opt$id])
  }))))
  
})
