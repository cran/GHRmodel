model_f <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))

# model_f$data is panel data (multiple spatial units per time point), so a
# single non-duplicated time series is needed to test paths that run before
# the 'unit'/'aggregate_space' checks (e.g. missing 'time' column, or
# 'aggregate_space' without 'unit').
single_rows <- model_f$data$micro_code == model_f$data$micro_code[1]
model_single <- model_f
model_single$data <- model_f$data[single_rows, ]
model_single$fitted <- lapply(model_f$fitted, function(x) x[single_rows, ])

# ────────────────────────────────────────────────
# config = FALSE model tests
# ────────────────────────────────────────────────

test_that("plot_fit returns ggplot object for base model", {
  p <- plot_fit(models = model_single, mod_id = "mod1", time = "date")
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit returns ggplot with two models and labels", {
  p <- plot_fit(
    models = model_single,
    mod_id = c("mod1", "mod3"),
    mod_label = c("mod1" = "Base", "mod3" = "Nonlinear"),
    time = "date",
    title = "Base vs Nonlinear"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit returns ggplot with CI ribbons", {
  p <- plot_fit(
    models = model_single,
    mod_id = "mod3",
    time = "date",
    ci = TRUE
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit handles log10p1 transform", {
  p <- plot_fit(
    models = model_single,
    mod_id = "mod2",
    time = "date",
    transform = "log10p1"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with unit faceting works", {
  p <- plot_fit(
    models = model_f,
    mod_id = "mod1",
    time = "date",
    unit = "micro_code"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with aggregate_space by column works", {
  p <- plot_fit(
    models = model_f,
    mod_id = "mod1",
    time = "date",
    unit = "micro_code",
    aggregate_space = "meso_code"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit with aggregate_space = 'all' works", {
  p <- plot_fit(
    models = model_f,
    mod_id = "mod1",
    time = "date",
    unit = "micro_code",
    aggregate_space = "all"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit handles xlim correctly", {
  p <- plot_fit(
    models = model_single,
    mod_id = "mod1",
    time = "date",
    xlim = as.Date(c("2010-01-01", "2015-12-31"))
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_fit errors for missing 'time' argument", {
  expect_error(
    plot_fit(models = model_single, mod_id = "mod1"),
    "Missing required argument 'time'"
  )
})

test_that("plot_fit errors when 'time' column is not found", {
  expect_error(
    plot_fit(models = model_single, mod_id = "mod1", time = "not_a_col"),
    "'time' not found in the data"
  )
})

test_that("plot_fit errors for invalid mod_id", {
  expect_error(
    plot_fit(models = model_single, mod_id = "not_a_model", time = "date"),
    "Unknown mod_id"
  )
})

test_that("plot_fit errors with malformed xlim (not a Date)", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = "mod1",
      time = "date",
      xlim = c("2010-01-01", "2015-12-31")
    ),
    "'xlim' must be of type date"
  )
})

test_that("plot_fit errors with xlim of wrong length", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = "mod1",
      time = "date",
      xlim = as.Date("2010-01-01")
    ),
    "must be a vector of length 2"
  )
})

test_that("plot_fit errors when xlim lower bound is later than upper", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = "mod1",
      time = "date",
      xlim = as.Date(c("2015-01-01", "2010-01-01"))
    ),
    "cannot be later than upper bound"
  )
})

test_that("plot_fit errors with invalid unit column", {
  expect_error(
    plot_fit(
      models = model_f,
      mod_id = "mod1",
      time = "date",
      unit = "bad_column"
    ),
    "No column of the data matches the 'unit' argument"
  )
})

test_that("plot_fit errors with aggregate_space but no unit", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = "mod1",
      time = "date",
      aggregate_space = "meso_code"
    ),
    "No 'unit' argument provided"
  )
})

test_that("plot_fit errors with invalid aggregate_space column", {
  expect_error(
    plot_fit(
      models = model_f,
      mod_id = "mod1",
      time = "date",
      unit = "micro_code",
      aggregate_space = "bad_column"
    ),
    "No column of the data match the 'aggregate_space' argument"
  )
})

test_that("plot_fit errors when combining ci with aggregate_space", {
  expect_error(
    plot_fit(
      models = model_f,
      mod_id = "mod1",
      time = "date",
      unit = "micro_code",
      aggregate_space = "meso_code",
      ci = TRUE
    ),
    "not available when using 'aggregate_space'"
  )
})

test_that("plot_fit errors when using aggregate_space with a gaussian model", {
  model_gaussian <- model_f
  model_gaussian$family[model_f$mod_gof$model_id == "mod1"] <- "gaussian"
  expect_error(
    plot_fit(
      models = model_gaussian,
      mod_id = "mod1",
      time = "date",
      unit = "micro_code",
      aggregate_space = "meso_code"
    ),
    "cannot be used with models with a gaussian likelihood"
  )
})

test_that("plot_fit errors with invalid transform", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = "mod1",
      time = "date",
      transform = "banana"
    ),
    "Invalid transform"
  )
})

test_that("plot_fit errors with mismatched mod_label length", {
  expect_error(
    plot_fit(
      models = model_single,
      mod_id = c("mod1", "mod2"),
      time = "date",
      mod_label = c("a", "b", "c")
    ),
    "same length as `mod_id`"
  )
})

test_that("plot_fit runs for all models in the object", {
  all_mods <- model_single$mod_gof$model_id
  for (m in all_mods) {
    expect_silent({
      p <- plot_fit(models = model_single, mod_id = m, time = "date")
      expect_s3_class(p, "ggplot")
    })
  }
})

# ────────────────────────────────────────────────
# Legend placement of observed data
# ────────────────────────────────────────────────

test_that("plot_fit keeps observed out of the model legend", {
  p <- plot_fit(
    models = model_single,
    mod_id = c("mod1", "mod3"),
    mod_label = c("mod1" = "Base", "mod3" = "Nonlinear"),
    time = "date",
    obs_label = "Cases",
    obs_color = "tomato"
  )

  # Model colour scale only contains models
  col_scale <- p$scales$get_scales("colour")
  expect_setequal(names(col_scale$labels), c("mod1", "mod3"))
  expect_equal(unname(col_scale$labels), c("Base", "Nonlinear"))

  # Observed has its own untitled legend scale with the requested label
  lt_scale <- p$scales$get_scales("linetype")
  expect_false(is.null(lt_scale))
  expect_equal(unname(lt_scale$labels), "Cases")
  expect_true(inherits(lt_scale$name, "waiver") || is.null(lt_scale$name))
})

test_that("plot_fit orders observed legend above model legend", {
  p <- plot_fit(
    models = model_single,
    mod_id = c("mod1", "mod3"),
    time = "date",
    obs_color = "tomato"
  )

  guides <- p$guides$guides
  expect_equal(guides$linetype$params$order, 1)
  expect_equal(guides$colour$params$order, 2)
  expect_equal(guides$linetype$params$override.aes$colour, "tomato")

  # Both legends are drawn when the plot is built
  expect_no_error(ggplot2::ggplotGrob(p))
})

# ────────────────────────────────────────────────
# config = TRUE model test (model_t)
# ────────────────────────────────────────────────

test_that("plot_fit works for a config = TRUE model", {
  single_rows_t <- model_t$data$micro_code == model_t$data$micro_code[1]
  model_t_single <- model_t
  model_t_single$data <- model_t$data[single_rows_t, ]
  model_t_single$fitted <- lapply(model_t$fitted, function(x) {
    x[single_rows_t, ]
  })

  p <- plot_fit(
    models = model_t_single,
    mod_id = "mod3",
    time = "date",
    ci = TRUE,
    transform = "log10p1",
    title = "Fitted vs Observed (config = TRUE)"
  )
  expect_s3_class(p, "ggplot")
})

# ────────────────────────────────────────────────
# unit_id tests
# ────────────────────────────────────────────────

test_that("plot_fit subsets data to unit_id", {
  ids <- unique(model_f$data$micro_code)[1:2]
  p <- plot_fit(
    models = model_f,
    mod_id = "mod1",
    time = "date",
    unit = "micro_code",
    unit_id = ids
  )
  expect_s3_class(p, "ggplot")
  expect_setequal(unique(p$data$unit), ids)
})

test_that("plot_fit errors on invalid unit_id usage", {
  expect_error(
    plot_fit(models = model_f, time = "date", unit_id = "a"),
    "'unit' is NULL"
  )
  expect_error(
    plot_fit(
      models = model_f,
      time = "date",
      unit = "micro_code",
      unit_id = "not_a_unit"
    ),
    "Invalid 'unit_id' values: not_a_unit"
  )
  expect_error(
    plot_fit(
      models = model_f,
      time = "date",
      unit = "micro_code",
      unit_id = character(0)
    ),
    "at least one value"
  )
  expect_error(
    plot_fit(
      models = model_f,
      time = "date",
      unit = "micro_code",
      unit_id = model_f$data$micro_code[1],
      aggregate_space = "all"
    ),
    "cannot be used"
  )
})
