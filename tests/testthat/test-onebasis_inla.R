test_that("onebasis_inla returns a onebasis object with correctly renamed columns", {
  skip_if_not_installed("dlnm")
  library(dlnm)
  
  set.seed(123)
  covariate <- rnorm(10, 20, 5)
  
  # Generate basis
  basis_name <- "myBasis"
  result <- onebasis_inla(covariate = covariate, fun = "bs", basis_name = basis_name, degree = 2)
  
  # Check the result class
  expect_s3_class(result, "onebasis")
  
  # Check that the column names match the expected pattern
  # 'b1' from the dlnm basis would turn into 'myBasis1', etc.
  col_names <- colnames(result)
  expect_true(all(grepl("^myBasis", col_names)))
})

test_that("onebasis_inla handles additional arguments and modifies column names", {
  skip_if_not_installed("dlnm")
  library(dlnm)
  
  set.seed(999)
  covariate <- rnorm(10, 15, 2)
  
  # Pass different additional arguments to dlnm::onebasis
  # For instance, specifying knots
  my_knots <- quantile(covariate, probs = c(0.25, 0.75))
  result <- onebasis_inla(
    covariate = covariate,
    fun = "bs",
    basis_name = "custom",
    knots = my_knots,
    degree = 1
  )
  
  # Verify class
  expect_s3_class(result, "onebasis")
  
  # Verify that columns start with 'custom'
  col_names <- colnames(result)
  expect_true(all(grepl("^custom", col_names)))
})
