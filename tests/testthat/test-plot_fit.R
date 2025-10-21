model_f <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))

# ────────────────────────────────────────────────
# config = FALSE model tests
# ────────────────────────────────────────────────

test_that("plot_fit returns ggplot object for base model", {
  p <- plot_fit(models = model_f,
                mod_id = "mod1",
                time = "date")
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit returns ggplot with two models and labels", {
  p <- plot_fit(models = model_f,
                mod_id = c("mod1", "mod3"),
                mod_label = c("mod1" = "Base", "mod3" = "Nonlinear"),
                time = "date",
                title = "Base vs Nonlinear")
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit returns ggplot with CI ribbons", {
  p <- plot_fit(models = model_f,
                mod_id = "mod3",
                time = "date",
                ci = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit handles log10p1 transform", {
  p <- plot_fit(models = model_f,
                mod_id = "mod2",
                time = "date",
                transform = "log10p1")
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with group faceting works", {
  p <- plot_fit(models = model_f,
                mod_id = "mod1",
                time = "date",
                group = "micro_code",
                group_id = c("50001", "50002"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with model faceting works", {
  p <- plot_fit(models = model_f,
                mod_id = c("mod1", "mod2"),
                time = "date",
                mod_facet = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with group and model faceting works", {
  p <- plot_fit(models = model_f,
                mod_id = c("mod1", "mod2"),
                time = "date",
                group = "micro_code",
                group_id = c("50001", "50002"),
                mod_facet = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit handles xlim correctly", {
  p <- plot_fit(models = model_f,
                mod_id = "mod1",
                time = "date",
                xlim = c("2010-01-01", "2015-12-31"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit errors for invalid mod_id", {
  expect_error(plot_fit(models = model_f,
                        mod_id = "not_a_model",
                        time = "date"),
               "Unknown mod_id")
})

test_that("plot_fit errors with malformed xlim", {
  expect_error(plot_fit(models = model_f,
                        mod_id = "mod1",
                        time = "date",
                        xlim = c("2010/01/01", "2015-12-31")),
               "must be in 'yyyy-mm-dd' format")
})

test_that("plot_fit errors with group_id but no group", {
  expect_error(plot_fit(models = model_f,
                        mod_id = "mod1",
                        time = "date",
                        group_id = "50001"),
               "`group` is NULL")
})

test_that("plot_fit errors with invalid group column", {
  expect_error(plot_fit(models = model_f,
                        mod_id = "mod1",
                        time = "date",
                        group = "bad_column"),
               "not found in models data")
})

test_that("plot_fit errors with invalid transform", {
  expect_error(plot_fit(models = model_f,
                        mod_id = "mod1",
                        time = "date",
                        transform = "banana"),
               "Invalid transform")
})

test_that("plot_fit runs for all models in the object", {
  all_mods <- model_f$mod_gof$model_id
  for (m in all_mods) {
    expect_silent({
      p <- plot_fit(models = model_f,
                    mod_id = m,
                    time = "date")
      expect_s3_class(p, "ggplot")
    })
  }
})

# ────────────────────────────────────────────────
# config = TRUE model test (model_t)
# ────────────────────────────────────────────────

test_that("plot_fit works for a config = TRUE model", {
  p <- plot_fit(models = model_t,
                mod_id = "mod3",
                time = "date",
                ci = TRUE,
                transform = "log10p1",
                title = "Fitted vs Observed (config = TRUE)")
  expect_s3_class(p, "ggplot")
})

