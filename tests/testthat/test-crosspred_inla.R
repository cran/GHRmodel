test_that("crosspred_inla works with fitted GHRmodels including crossbasis", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  cp_result <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = cb_tmin,
    mod_id = mod_id,
    at = seq(10, 30, by = 1),
    lag = c(1, 6),
    cen = 20
  ))

  expect_s3_class(cp_result, "GHRcrosspred")
  expect_s3_class(cp_result, "crosspred")
  expect_true(all(
    c("predvar", "allfit", "alllow", "allhigh") %in% names(cp_result)
  ))
  expect_equal(length(cp_result$predvar), length(seq(10, 30, by = 1)))
})

test_that("crosspred_inla works with fitted GHRmodels including onebasis", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[4]
  data <- mod$data

  ob_urb <- onebasis_inla(
    covariate = data$urban,
    fun = "bs",
    degree = 3,
    basis_name = "urban"
  )

  cp_result <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = ob_urb,
    mod_id = mod_id,
    at = seq(0, 100, by = 10),
    cen = 20
  ))

  expect_s3_class(cp_result, "GHRcrosspred")
  expect_s3_class(cp_result, "crosspred")
  expect_true(all(
    c("predvar", "allfit", "alllow", "allhigh") %in% names(cp_result)
  ))
  expect_equal(length(cp_result$predvar), length(seq(0, 100, by = 10)))
})


test_that("crosspred_inla returns cumulative results when requested", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  cp_result <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = cb_tmin,
    mod_id = mod_id,
    at = seq(10, 30, by = 2),
    lag = c(1, 6),
    cen = 20,
    cumul = TRUE
  ))

  expect_s3_class(cp_result, "GHRcrosspred")
  expect_true("cumfit" %in% names(cp_result))
  expect_true("cumlow" %in% names(cp_result))
  expect_true("cumhigh" %in% names(cp_result))
})

test_that("crosspred_inla works with from-to-by range instead of at", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  cp_result <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = cb_tmin,
    mod_id = mod_id,
    from = 5,
    to = 25,
    by = 5,
    lag = c(1, 6),
    cen = 15
  ))

  expect_s3_class(cp_result, "GHRcrosspred")
  expect_equal(cp_result$predvar, seq(5, 25, by = 5))
})

test_that("crosspred_inla throws error if no matching coefficients found", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]

  # Create a dummy basis with column names that don't match coefficient names
  bad_basis <- matrix(rnorm(60), nrow = 10)
  colnames(bad_basis) <- paste0("nonmatch", seq_len(ncol(bad_basis)))

  expect_error(
    suppressWarnings(
      crosspred_inla(
        models = mod,
        basis = bad_basis,
        mod_id = mod_id,
        at = seq(5, 20, by = 5),
        lag = c(1, 6),
        cen = 10
      ),
      regexp = "arguments 'basis' and 'model' not consistent\\. See help\\(crosspred\\)"
    )
  )
})


test_that("crosspred_inla throws error with invalid lag input", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[1]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  expect_error(
    crosspred_inla(
      models = mod,
      basis = cb_tmin,
      mod_id = mod_id,
      at = seq(5, 20, by = 5),
      lag = NULL,
      cen = 15
    ),
    regexp = "'lag' must be a vector of non-duplicate positive integers of length 2.",
    fixed = FALSE
  )
})


test_that("crosspred_inla throws error if GHRmodels were fitted with vcov = FALSE", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_f.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  expect_error(
    crosspred_inla(
      models = mod,
      basis = cb_tmin,
      mod_id = mod_id,
      at = seq(5, 20, by = 5),
      lag = c(1, 6),
      cen = 15
    ),
    regexp = "Variance-covariance matrix \\(vcov\\) for 'mod_id' is NULL"
  )
})

test_that("crosspred_inla is robust to fixed/vcov row order not matching basis column order", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  cp_reference <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = cb_tmin,
    mod_id = mod_id,
    at = seq(10, 30, by = 2),
    lag = c(1, 6),
    cen = 20
  ))

  # Reverse the row/column order of the fixed-effects table and vcov matrix
  # for this model, without changing any values -- predictions should be
  # unaffected since crosspred_inla must realign by name, not by position.
  shuffled_mod <- mod
  perm <- rev(seq_len(nrow(mod$fixed[[mod_id]])))
  shuffled_mod$fixed[[mod_id]] <- mod$fixed[[mod_id]][perm, , drop = FALSE]
  shuffled_mod$vcov[[mod_id]] <- mod$vcov[[mod_id]][perm, perm, drop = FALSE]

  cp_shuffled <- suppressWarnings(crosspred_inla(
    models = shuffled_mod,
    basis = cb_tmin,
    mod_id = mod_id,
    at = seq(10, 30, by = 2),
    lag = c(1, 6),
    cen = 20
  ))

  expect_equal(cp_shuffled$allfit, cp_reference$allfit)
  expect_equal(cp_shuffled$allse, cp_reference$allse)
})

test_that("crosspred_inla throws an informative error when only some basis columns match", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  # Corrupt one column name so it no longer matches any model coefficient,
  # while the rest of the basis columns still match.
  partial_basis <- cb_tmin
  colnames(partial_basis)[1] <- "nonmatch1"

  expect_error(
    crosspred_inla(
      models = mod,
      basis = partial_basis,
      mod_id = mod_id,
      at = seq(10, 30, by = 2),
      lag = c(1, 6),
      cen = 20
    ),
    regexp = "Basis columns not found among model coefficients: nonmatch1"
  )
})

test_that("crosspred_inla throws an error if mod_id does not have length 1", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  expect_error(
    crosspred_inla(
      models = mod,
      basis = cb_tmin,
      mod_id = names(mod$fixed)[1:2],
      at = seq(10, 30, by = 2),
      lag = c(1, 6),
      cen = 20
    ),
    regexp = "'mod_id' must have length 1"
  )
})

test_that("crosspred_inla falls back to the basis's fitted lag range when lag is omitted", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  cp_result <- suppressWarnings(crosspred_inla(
    models = mod,
    basis = cb_tmin,
    mod_id = mod_id,
    at = seq(10, 30, by = 2),
    cen = 20
  ))

  expect_equal(cp_result$lag, attr(cb_tmin, "lag"))
})

test_that("crosspred_inla throws an error if basis is not a valid basis object", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]

  expect_error(
    crosspred_inla(
      models = mod,
      basis = data.frame(a = 1, b = 2),
      mod_id = mod_id,
      at = seq(10, 30, by = 2),
      lag = c(1, 6),
      cen = 20
    ),
    regexp = "'basis' must be a cross-basis or one-basis object"
  )
})

test_that("crosspred_inla throws an error if models is not a GHRmodels object", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data

  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1, 6),
    argvar = list(fun = "bs"),
    arglag = list(fun = "bs"),
    basis_name = "tmin"
  )

  expect_error(
    crosspred_inla(
      models = unclass(mod),
      basis = cb_tmin,
      mod_id = mod_id,
      at = seq(10, 30, by = 2),
      lag = c(1, 6),
      cen = 20
    ),
    regexp = "'models' must be an object of class 'GHRmodels'"
  )
})
