test_that("plot_coef_crosspred works with all types using crosspred_inla", {
  
  # Load model and data
  mod <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  mod_id <- names(mod$fixed)[2]
  data <- mod$data
  
  # Create crossbasis and crosspred
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
  
  # Overall plot
  p_overall <- plot_coef_crosspred(cp_result, type = "overall")
  expect_s3_class(p_overall, "ggplot")
  
  # Slices by var
  p_var_slice <- plot_coef_crosspred(cp_result, type = "slices", var = c(15, 20, 25))
  expect_s3_class(p_var_slice, "ggplot")
  
  # Slices by lag
  p_lag_slice <- plot_coef_crosspred(cp_result, type = "slices", lag = c(1, 3, 6))
  expect_s3_class(p_lag_slice, "ggplot")
  
  # Heatmap
  p_heatmap <- plot_coef_crosspred(cp_result, type = "heatmap")
  expect_s3_class(p_heatmap, "ggplot")
})

test_that("plot_coef_crosspred errors correctly", {
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
  
  # Both var and lag provided
  expect_error(
    plot_coef_crosspred(cp_result, type = "slices", var = 15, lag = 2),
    "Provide only one of 'var' or 'lag'"
  )
  
  # Neither var nor lag provided
  expect_error(
    plot_coef_crosspred(cp_result, type = "slices"),
    "Specify either 'var' or 'lag'"
  )
  
  # Invalid object class
  expect_error(
    plot_coef_crosspred(list(), type = "overall"),
    "'crosspred' must be of class 'crosspred'"
  )
})

