test_that("crosspred_inla works with fitted GHRmodels including crossbasis", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data
  
  cb_tmin <- crossbasis_inla(
    covariate = data[, grep("tmin.l", colnames(data))],
    lag = c(1,6),
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
  expect_true(all(c("predvar", "allfit", "alllow", "allhigh") %in% names(cp_result)))
  expect_equal(length(cp_result$predvar), length(seq(10, 30, by = 1)))
})

test_that("crosspred_inla works with fitted GHRmodels including onebasis", {
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[4]
  data <- mod$data
  
  ob_urb <- onebasis_inla(
    covariate = data$urban,
    fun = "bs", degree = 3,
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
  expect_true(all(c("predvar", "allfit", "alllow", "allhigh") %in% names(cp_result)))
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
  
  # Create a dummy basis with column names that don't match model coefficient names
  bad_basis <- matrix(rnorm(60), nrow = 10)
  colnames(bad_basis) <- paste0("nonmatch", 1:ncol(bad_basis))
  
  expect_error(
    suppressWarnings(crosspred_inla(
      models = mod,
      basis = bad_basis,
      mod_id = mod_id,
      at = seq(5, 20, by = 5),
      lag = c(1, 6),
      cen = 10
    ),
    regexp = "arguments 'basis' and 'model' not consistent\\. See help\\(crosspred\\)"
  ))
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
    regexp = "No matching coefficients found for provided basis.",
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



