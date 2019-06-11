context("Test that make_random_draws() returns correct dimensions")

test_that("make_random_draws() returns correct dimension", {
    N <- 10
    R <- 5
    D <- 3
    
    draws_01 <- make_random_draws(N, R, D, "pseudo_random")
    draws_02 <- make_random_draws(N, R, D, "mlhs")
    draws_03 <- make_random_draws(N, R, D, "standard_halton")
    draws_04 <- make_random_draws(N, R, D, "scrambled_halton")
    draws_05 <- make_random_draws(N, R, D, "standard_sobol")
    draws_06 <- make_random_draws(N, R, D, "scrambled_sobol")
    
    expect_true(all(dim(draws_01) == c(50, 3)))
    expect_true(all(dim(draws_02) == c(50, 3)))
    expect_true(all(dim(draws_03) == c(50, 3)))
    expect_true(all(dim(draws_04) == c(50, 3)))
    expect_true(all(dim(draws_05) == c(50, 3)))
    expect_true(all(dim(draws_06) == c(50, 3)))
    

})