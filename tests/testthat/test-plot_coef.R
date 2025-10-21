
model_f <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))

# PLOT_COEF_LIN ------

# Basic valid use cases

test_that("plot_coef_lin works with one model (config = TRUE)", {
  p <- plot_coef_lin(models = model_t, mod_id = "mod2")
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_lin works with two models and mod_label", {
  p <- plot_coef_lin(
    models = model_f,
    mod_id = c("mod2", "mod4"),
    mod_label = c("mod2" = "Base", "mod4" = "Covariates")
  )
  expect_s3_class(p, "ggplot")
})

# Filtering: exact variables

test_that("plot_coef_lin filters by exact variable (tmin.l1)", {
  p <- plot_coef_lin(models = model_f, mod_id = "mod2", name = "tmin.l2")
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_lin filters by multiple variables (mod5)", {
  p <- plot_coef_lin(models = model_f, mod_id = "mod5", name = c("pdsi.l2"))
  expect_s3_class(p, "ggplot")
})

# Pattern filtering

test_that("plot_coef_lin filters by pattern (pdsi)", {
  p <- plot_coef_lin(models = model_f, mod_id = "mod7", pattern = "pdsi")
  expect_s3_class(p, "ggplot")
})

# Labeling

test_that("plot_coef_lin applies custom mod_label and var_label", {
  p <- plot_coef_lin(
    models = model_f,
    mod_id = c("mod2", "mod5"),
    name = "tmin.l2",
    var_label = c("tmin.l2" = "Min temp"),
    mod_label = c("mod2" = "Crude", "mod5" = "Adjusted")
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_lin supports var_label with interaction term", {
  p <- plot_coef_lin(
    models = model_f,
    mod_id = "mod8",
    name = c("pdsi.l2:tmin.l2"),
    var_label = c("pdsi.l2:tmin.l2" = "PDSI × Tmin")
  )
  expect_s3_class(p, "ggplot")
})

# Expected errors

test_that("plot_coef_lin errors if mod_id is invalid", {
  expect_error(plot_coef_lin(models = model_f, mod_id = "nonexistent"), "not found")
})

test_that("plot_coef_lin give warnings if variable not in model", {
  expect_warning(
    plot_coef_lin(models = model_f, mod_id = "mod4", name = "tmin.l2"),
    "No linear effects matched by name/pattern found in model"
  )
})

test_that("plot_coef_lin give warnings if variable not in model", {
  expect_warning(
    plot_coef_lin(models = model_f, mod_id = "mod4"),
    "No linear effects matched by name/pattern found in model"
  )
})

test_that("plot_coef_lin errors for model with intercept only (mod1)", {
  expect_warning(
    plot_coef_lin(models = model_f, mod_id = "mod1"),
    "No linear effects matched by name/pattern found in model"
  )
})

test_that("plot_coef_lin errors with unnamed mod_label", {
  expect_error(
    plot_coef_lin(models = model_f, mod_id = c("mod2", "mod4"), mod_label = c("A", "B")),
    "'mod_label' must be a named vector"
  )
})



# PLOT_COEF_NL ---------

test_that("plot_coef_nl works with one nonlinear term (config = TRUE)", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = "mod3",
                    name = "tmin.l2")
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_nl works with multiple models and collapse = TRUE", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = c("mod3", "mod6"),
                    name = c("tmin.l2"),
                    mod_label = c("mod3" = "Crude", "mod6" = "Adjusted"),
                    collapse = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_nl works with multiple models (collapse = FALSE)", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = c("mod3", "mod6"),
                    name = c("tmin.l2"),
                    mod_label = c("mod3" = "Crude", "mod6" = "Adjusted"),
                    collapse = FALSE)
  expect_s3_class(p, "ggplot")
})

# Filtering by pattern

test_that("plot_coef_nl supports pattern-based selection", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = "mod6",
                    pattern = "tmin",
                    collapse = FALSE)
  expect_s3_class(p, "ggplot")
})

# Faceting, histogram, axis labels

test_that("plot_coef_nl supports title, var_label, xlim", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = "mod6",
                    name = "tmin.l2",
                    var_label = c("tmin.l2" = "Temperature (lag 1)"),
                    title = "Nonlinear effect",
                    xlim = list("tmin.l2" = c(15, 25)))
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_nl disables histogram if histogram = FALSE", {
  p <- plot_coef_nl(models = model_t,
                    mod_id = "mod6",
                    name = "tmin.l2",
                    histogram = TRUE)
  expect_s3_class(p, "ggplot")
})

# Error handling

test_that("plot_coef_nl warns when variable is not found", {
  expect_warning(
    plot_coef_nl(models = model_t, mod_id = "mod2", name = "not_a_var"),
    regexp = "Variable 'not_a_var' not found in any models"
  )
})

test_that("plot_coef_nl errors when pattern doesn't match", {
  expect_warning(
    plot_coef_nl(models = model_t, mod_id = "mod5", pattern = "xyz"),
    regexp = "Pattern 'xyz' not found in any models"
  )
})

test_that("plot_coef_nl warns when no nonlinear effects", {
  expect_warning(
    plot_coef_nl(models = model_t, mod_id = "mod2"),
    "No nonlinear effects found in model"
  )
})

test_that("plot_coef_nl errors if collapse = TRUE and replicated effect", {
  expect_error(
    plot_coef_nl(models = model_t,
                 mod_id = c("mod3", "mod4"),
                 name = "tmin.l2",
                 collapse = TRUE),
    "Cannot collapse nonlinear effects of multiple models when one of the effects is replicated"
  )
})

test_that("plot_coef_nl errors if var is missing in collapse mode", {
  expect_error(
    plot_coef_nl(models = model_t,
                 mod_id = c("mod2", "mod5"),
                 collapse = TRUE),
    "'name' must be a single variable name."
  )
})


# PLOT_COEF_VARYING -------

# Basic valid use cases

test_that("plot_coef_varying works for a varying slope (config = FALSE)", {
  p <- plot_coef_varying(
    models = model_f,
    mod_id = "mod9",
    name = "main_climate_f"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_varying works for a varying slope (config = TRUE)", {
  p <- plot_coef_varying(
    models = model_t,
    mod_id = "mod9",
    name = "main_climate_f"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_coef_varying supports var_label mapping (config = TRUE)", {
  # Get levels from data
  ids <- sort(unique(model_t$data$main_climate_f))
  labels <- setNames(paste("Group", ids), ids)
  
  p <- plot_coef_varying(
    models = model_t,
    mod_id = "mod9",
    name = "main_climate_f",
    unit_label = labels,
    title = "Grouped Effect",
    xlab = "Effect Estimate"
  )
  expect_s3_class(p, "ggplot")
})

# Error handling

test_that("plot_coef_varying errors if mod_id not found", {
  expect_error(
    plot_coef_varying(models = model_f, mod_id = "modX", name = "main_climate_f"),
    "not found in 'GHRmodels'"
  )
})

test_that("plot_coef_varying errors if var not found", {
  expect_warning(
    plot_coef_varying(models = model_f, mod_id = "mod2", name = "not_a_var"),
    "No varying effects term matching 'not_a_var' in model 'mod2'."
  )
})

test_that("plot_coef_varying errors with multiple var inputs", {
  expect_error(
    plot_coef_varying(models = model_f, mod_id = "mod9", name = c("a", "b")),
    "Please supply exactly one variable name"
  )
})

test_that("plot_coef_varying errors with unnamed var_label", {
  lbls <- c("A", "B", "C")
  expect_error(
    plot_coef_varying(models = model_t,
                      mod_id = "mod9",
                      name = "main_climate_f",
                      unit_label = lbls),
    "'unit_label' must be a named vector"
  )
})

test_that("plot_coef_varying errors if label missing for IDs", {
  ids <- sort(unique(model_t$data$main_climate_f))
  bad_lbls <- setNames(paste("Label", ids[-1]), ids[-1])  # omit first ID
  expect_error(
    plot_coef_varying(models = model_t,
                      mod_id = "mod9",
                      name = "main_climate_f",
                      unit_label = bad_lbls),
    "Missing labels for the following IDs: 1"
  )
})

