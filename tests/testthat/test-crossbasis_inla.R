test_that("crossbasis_inla returns crossbasis_inla object", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(123)
  # Create a small example matrix with 5 observations and 3 lags (lag 0-2)
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  # Create crossbasis_inla object
  basis_name <- "tempLag"
  cb_inla <- crossbasis_inla(
    covariate = covariate,
    basis_name = basis_name,
    lag = c(0, 2),
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
  # Another small matrix, 5 lags (lag 0-4)
  covariate <- matrix(rnorm(1500, mean = 25, sd = 5), nrow = 300, ncol = 5)

  # Provide different arguments for argvar and arglag
  basis_name <- "myPrefix"
  result <- crossbasis_inla(
    covariate = covariate,
    basis_name = basis_name,
    lag = c(0, 4),
    argvar = list(fun = "poly", degree = 2),
    arglag = list(fun = "bs", degree = 3)
  )

  # Check classes
  expect_s3_class(result, "crossbasis_inla")
  expect_s3_class(result, "crossbasis")

  # Column names start with the correct prefix
  expect_true(all(grepl(paste0("^", basis_name), colnames(result))))

  # Optionally, you could check dimensions:
  # Number of columns in a crossbasis is typically
  # (# of basis var) * (# of basis lag).
  # The exact dimension depends on the underlying basis expansions.
  # For a quick dimension check (though not strictly required):
  expect_true(ncol(result) > 0)
  expect_equal(nrow(result), nrow(covariate))
})

test_that("crossbasis_inla renames columns exactly, with no leftover 'v' prefix", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(42)
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  raw <- dlnm::crossbasis(
    x = covariate,
    lag = c(0, 2),
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )
  expected_names <- sub("^v", "tempLag", colnames(raw))

  cb_inla <- crossbasis_inla(
    covariate = covariate,
    basis_name = "tempLag",
    lag = c(0, 2),
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )

  expect_identical(colnames(cb_inla), expected_names)
  expect_false(any(grepl("^v[0-9]", colnames(cb_inla))))
})

test_that("crossbasis_inla accepts a data.frame covariate", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(7)
  covariate_df <- as.data.frame(matrix(
    rnorm(15, mean = 20, sd = 5),
    nrow = 5,
    ncol = 3
  ))

  cb_inla <- crossbasis_inla(
    covariate = covariate_df,
    basis_name = "tempLag",
    lag = c(0, 2),
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )

  expect_s3_class(cb_inla, "crossbasis_inla")
  expect_equal(nrow(cb_inla), nrow(covariate_df))
})

test_that("crossbasis_inla sorts and deduplicates an unordered lag vector", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(11)
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  cb_sorted <- crossbasis_inla(
    covariate = covariate,
    basis_name = "tempLag",
    lag = c(0, 2),
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )
  cb_unsorted <- crossbasis_inla(
    covariate = covariate,
    basis_name = "tempLag",
    lag = c(2, 0),
    argvar = list(fun = "bs", df = 3),
    arglag = list(fun = "poly", degree = 2)
  )

  expect_identical(unclass(cb_sorted), unclass(cb_unsorted))
})

test_that("crossbasis_inla passes extra '...' arguments through to dlnm::crossbasis", {
  skip_if_not_installed("dlnm")
  library(dlnm)

  set.seed(21)
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  # dlnm:::checkcrossbasis() rejects a fixed set of deprecated argument
  # *values* (a quirk of how it inspects '...'), so getting this error back
  # proves that '...' is actually forwarded to dlnm::crossbasis rather than
  # swallowed by crossbasis_inla().
  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2),
      dummy = "vardf"
    ),
    "old usage not allowed any more"
  )
})

test_that("crossbasis_inla validates 'covariate'", {
  expect_error(
    crossbasis_inla(
      covariate = 1:5,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'covariate' must be a matrix or data.frame"
  )

  char_mat <- matrix(letters[1:15], nrow = 5, ncol = 3)
  expect_error(
    crossbasis_inla(
      covariate = char_mat,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'covariate' must contain numeric values"
  )

  expect_error(
    crossbasis_inla(
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    )
  )
})

test_that("crossbasis_inla validates 'basis_name'", {
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = 123,
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'basis_name' must be a non-empty character string"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'basis_name' must be a non-empty character string"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = c("a", "b"),
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'basis_name' must be a non-empty character string"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    )
  )
})

test_that("crossbasis_inla validates 'lag'", {
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = "a",
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'lag' must be a vector of"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(-1, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'lag' must be a vector of"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0.5, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'lag' must be a vector of"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = 2,
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'lag' must be a vector of"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 1, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'lag' must be a vector of"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      argvar = list(fun = "bs", df = 3),
      arglag = list(fun = "poly", degree = 2)
    )
  )
})

test_that("crossbasis_inla validates 'argvar' and 'arglag'", {
  covariate <- matrix(rnorm(15, mean = 20, sd = 5), nrow = 5, ncol = 3)

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = "bs",
      arglag = list(fun = "poly", degree = 2)
    ),
    "'argvar' must be a non-empty list"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(),
      arglag = list(fun = "poly", degree = 2)
    ),
    "'argvar' must be a non-empty list"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = "poly"
    ),
    "'arglag' must be a non-empty list"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3),
      arglag = list()
    ),
    "'arglag' must be a non-empty list"
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      arglag = list(fun = "poly", degree = 2)
    )
  )

  expect_error(
    crossbasis_inla(
      covariate = covariate,
      basis_name = "tempLag",
      lag = c(0, 2),
      argvar = list(fun = "bs", df = 3)
    )
  )
})
