test_that("sample_ppd works with valid GHRmodels (config = TRUE)", {
  
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")
  
  data("map_MS")
  nb <- spdep::poly2nb(map_MS)
  assign("g", spdep::nb2mat(nb, style = "B"), envir = globalenv())
  
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  assign("prior_re1", list(prec = list(prior = 'loggamma', 
                                       param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re2", list(prec = list(prior = 'loggamma',
                                       param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re3", list(
    prec = list(prior = 'pc.prec', param = c(0.5 / 0.31, 0.01)),
    phi  = list(prior = 'pc', param = c(0.5, 2 / 3))
  ), envir = globalenv())
  
  valid_mod_id <- model_t$mod_gof$model_id[1]
  result <- sample_ppd(models = model_t, mod_id = valid_mod_id, s = 10, nthreads = 2)
  
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 11)
  expect_true("observed" %in% names(result))
  expect_equal(nrow(result), nrow(model_t$data))
  
  rm(list = c("prior_re1", "prior_re2", "prior_re3", "g"), envir = globalenv())
})


test_that("plot_ppd works for a correct ppd object", {
  
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")
  
  data("map_MS")
  nb <- spdep::poly2nb(map_MS)
  assign("g", spdep::nb2mat(nb, style = "B"), envir = globalenv())
  
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  assign("prior_re1", list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re2", list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re3", list(
    prec = list(prior = 'pc.prec', param = c(0.5 / 0.31, 0.01)),
    phi  = list(prior = 'pc', param = c(0.5, 2 / 3))
  ), envir = globalenv())
  
  valid_mod_id <- model_t$mod_gof$model_id[1]
  result <- sample_ppd(models = model_t, mod_id = valid_mod_id, s = 10, nthreads = 2)
  
  p <- plot_ppd(result)
  expect_s3_class(p, "ggplot")
  
  rm(list = c("prior_re1", "prior_re2", "prior_re3", "g"), envir = globalenv())
})


test_that("plot_ppd throws error for invalid xlim input", {
  
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")
  
  data("map_MS")
  nb <- spdep::poly2nb(map_MS)
  assign("g", spdep::nb2mat(nb, style = "B"), envir = globalenv())
  
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  assign("prior_re1", list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re2", list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))), envir = globalenv())
  assign("prior_re3", list(
    prec = list(prior = 'pc.prec', param = c(0.5 / 0.31, 0.01)),
    phi  = list(prior = 'pc', param = c(0.5, 2 / 3))
  ), envir = globalenv())
  
  valid_mod_id <- model_t$mod_gof$model_id[1]
  result <- sample_ppd(models = model_t, mod_id = valid_mod_id, s = 10, nthreads = 2)
  
  expect_error(
    plot_ppd(result, xlim = "invalid"),
    "`xlim` must be a numeric vector of length 2"
  )
  expect_error(
    plot_ppd(result, xlim = c(1)),
    "`xlim` must be a numeric vector of length 2"
  )
  
  rm(list = c("prior_re1", "prior_re2", "prior_re3", "g"), envir = globalenv())
})
