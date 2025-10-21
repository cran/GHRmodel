test_that("crossbasis_inla returns an object of class crossbasis_inla with the correct column names", {
  skip_if_not_installed("dlnm")
  library(dlnm)
  
  set.seed(123)
  # Create a small example matrix with 5 observations and 3 lags (for demonstration)
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)
  
  # Create crossbasis_inla object
  basis_name <- "tempLag"
  lag <- 2
  cb_inla <- crossbasis_inla(
    covariate  = covariate,
    basis_name = basis_name,
    lag        = lag,
    # Minimal argvar and arglag for demonstration
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )
  
  # Test that cb_inla inherits the new class
  expect_true(inherits(cb_inla, "crossbasis_inla"))
  # And also inherits from "crossbasis"
  expect_true(inherits(cb_inla, "crossbasis"))
  
  # Test that column names are prefixed with the desired basis_name
  col_names <- colnames(cb_inla)
  expect_true(all(grepl(paste0("^", basis_name), col_names)))
})

test_that("crossbasis_inla handles additional arguments to crossbasis", {
  skip_if_not_installed("dlnm")
  library(dlnm)
  
  set.seed(999)
  # Another small matrix
  covariate <- matrix(rnorm(1500, mean = 25, sd = 5), nrow = 300, ncol = 5)
  
  # Provide different arguments for argvar and arglag
  basis_name <- "myPrefix"
  result <- crossbasis_inla(
    covariate  = covariate,
    basis_name = basis_name,
    lag        = 4,
    argvar     = list(fun = "poly", degree = 2),
    arglag     = list(fun = "bs", degree = 3)
  )
  
  # Check classes
  expect_s3_class(result, "crossbasis_inla")
  expect_s3_class(result, "crossbasis")
  
  # Column names start with the correct prefix
  expect_true(all(grepl(paste0("^", basis_name), colnames(result))))
  
  # Optionally, you could check dimensions:
  # Number of columns in a crossbasis is typically (# of basis var) * (# of basis lag).
  # The exact dimension depends on the underlying basis expansions.
  # For a quick dimension check (though not strictly required):
  expect_true(ncol(result) > 0)
  expect_equal(nrow(result), nrow(covariate))
})
