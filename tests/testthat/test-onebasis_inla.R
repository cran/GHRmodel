test_that("onebasis_inla returns a onebasis object with correct columns", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(123)
  covariate <- rnorm(10, 20, 5)

  # Generate basis
  basis_name <- "myBasis"
  result <- onebasis_inla(
    covariate = covariate,
    fun = "bs",
    basis_name = basis_name,
    degree = 2
  )

  # Check the result class
  expect_s3_class(result, "onebasis")

  # Check that the column names match the expected pattern
  # 'b1' from the dlnm basis would turn into 'myBasis1', etc.
  col_names <- colnames(result)
  expect_true(all(grepl("^myBasis", col_names)))
})

test_that("onebasis_inla handles additional args and modifies column names", {
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

test_that("onebasis_inla is behaviorally equivalent to dlnm::onebasis", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(42)
  covariate <- rnorm(15, 20, 5)

  reference <- dlnm::onebasis(x = covariate, fun = "bs", degree = 2)
  result <- onebasis_inla(
    covariate = covariate,
    fun = "bs",
    basis_name = "temp",
    degree = 2
  )

  # Values are unchanged, only column names differ
  expect_equal(unname(result), unname(reference))
  expect_equal(colnames(result), sub("^b", "temp", colnames(reference)))

  # Other attributes are preserved as-is
  expect_equal(attr(result, "fun"), attr(reference, "fun"))
  expect_equal(attr(result, "range"), attr(reference, "range"))
  expect_equal(attr(result, "intercept"), attr(reference, "intercept"))
})

test_that("onebasis_inla renames columns for different fun types", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(7)
  covariate <- rnorm(20, 20, 5)

  for (fn in c("lin", "ns", "poly", "strata")) {
    result <- onebasis_inla(covariate = covariate, fun = fn, basis_name = "cov")
    expect_true(all(grepl("^cov", colnames(result))), info = fn)
  }
})

test_that("onebasis_inla validates the covariate argument", {
  expect_error(
    onebasis_inla(covariate = "not numeric", fun = "bs", basis_name = "b"),
    "'covariate' must be a numeric vector"
  )
  expect_error(
    onebasis_inla(
      covariate = matrix(rnorm(10), ncol = 2),
      fun = "bs",
      basis_name = "b"
    ),
    "'covariate' must be a numeric vector"
  )
  expect_error(
    onebasis_inla(covariate = numeric(0), fun = "bs", basis_name = "b"),
    "'covariate' must be a numeric vector"
  )
  expect_error(
    onebasis_inla(
      covariate = c(NA_real_, NA_real_),
      fun = "bs",
      basis_name = "b"
    ),
    "'covariate' must have non-NA elements"
  )
})

test_that("onebasis_inla validates the fun argument", {
  covariate <- rnorm(10, 20, 5)

  expect_error(
    onebasis_inla(covariate = covariate, fun = 1, basis_name = "b"),
    "'fun' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(covariate = covariate, fun = c("bs", "ns"), basis_name = "b"),
    "'fun' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(covariate = covariate, fun = "", basis_name = "b"),
    "'fun' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(covariate = covariate, fun = NA_character_, basis_name = "b"),
    "'fun' must be a non-empty character string"
  )
})

test_that("onebasis_inla validates the basis_name argument", {
  covariate <- rnorm(10, 20, 5)

  expect_error(
    onebasis_inla(covariate = covariate, fun = "bs", basis_name = 1),
    "'basis_name' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(covariate = covariate, fun = "bs", basis_name = c("a", "b")),
    "'basis_name' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(covariate = covariate, fun = "bs", basis_name = ""),
    "'basis_name' must be a non-empty character string"
  )
  expect_error(
    onebasis_inla(
      covariate = covariate,
      fun = "bs",
      basis_name = NA_character_
    ),
    "'basis_name' must be a non-empty character string"
  )
})
