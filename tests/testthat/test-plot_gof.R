model_f <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))

# --- CONFIG = FALSE (ghr_model_config_f.rds) ----------------------

test_that("plot_gof works with dic and mae with config = FALSE", {
  expect_s3_class(plot_gof(model_f$mod_gof, metric = "dic"), "ggplot")
  expect_s3_class(plot_gof(model_f$mod_gof, metric = "mae"), "ggplot")
  expect_s3_class(plot_gof(model_f$mod_gof, metric = "rmse"), "ggplot")
})

test_that("plot_gof empty plot for crps in config = FALSE", {
  expect_s3_class(plot_gof(model_f$mod_gof, metric = "crps"), "ggplot")
})

# --- CONFIG = TRUE (ghr_model_config_t.rds) -----------------------

test_that("plot_gof works with dic, mae, rmse, and crps with config = TRUE", {
  expect_s3_class(plot_gof(model_t$mod_gof, metric = "dic"), "ggplot")
  expect_s3_class(plot_gof(model_t$mod_gof, metric = "mae"), "ggplot")
  expect_s3_class(plot_gof(model_t$mod_gof, metric = "rmse"), "ggplot")
  expect_s3_class(plot_gof(model_t$mod_gof, metric = "crps"), "ggplot")
})

test_that("plot_gof with CI bars for dic_vs_first", {
  expect_s3_class(plot_gof(model_t$mod_gof, metric = "dic_vs_first", ci = TRUE), "ggplot")
})

test_that("plot_gof with color, shape and facet works", {
  p <- plot_gof(
    mod_gof = model_t$mod_gof,
    metric = "waic",
    var_color = "covariate_1",
    var_shape = "covariate_2",
    var_facet = "covariate_3",
    palette = "IDE2"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_gof respects custom mod_label vector", {
  ids <- model_t$mod_gof$model_id
  custom_labels <- stats::setNames(paste("Model", seq_along(ids)), ids)
  
  p <- plot_gof(
    mod_gof = model_t$mod_gof,
    metric = "waic",
    mod_label = custom_labels
  )
  expect_s3_class(p, "ggplot")
})

