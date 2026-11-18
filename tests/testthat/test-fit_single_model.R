# Load example data
data("dengue_MS", package = "GHRmodel")
data("map_MS", package = "GHRmodel")

# Filter data for years after 2015
dengue_MS <- dplyr::filter(dengue_MS, year > 2016)

# Define offset and factor IDs
dengue_MS$offset <- dengue_MS$population
dengue_MS$month_id <- as.numeric(as.factor(dengue_MS$month))
dengue_MS$year_id <- as.numeric(as.factor(dengue_MS$year))
dengue_MS$spat_id <- as.numeric(as.factor(dengue_MS$micro_code))

test_that("fit_single_model runs on dengue_MS example with filtered years", {
  # Ensure INLA is available
  skip_if_not_installed("INLA")

  # Create a single INLA formula
  single_form <- write_inla_formulas(
    outcome = "dengue_cases",
    covariates = NULL,
    re1 = list(
      id = "month_id",
      model = "rw1",
      cyclic = TRUE
    ),
    re2 = list(
      id = "year_id",
      model = "rw1"
    ),
    re3 = list(
      id = "spat_id",
      model = "iid"
    ),
    baseline = TRUE
  )

  # Fit the model using fit_single_model
  model <- .fit_single_model(
    formula = single_form,
    data = dengue_MS,
    family = "nbinomial",
    offset_values = dengue_MS$pop100000,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = 4
  )

  # Verify we get a valid INLA model object
  expect_s3_class(model, "inla")
})

test_that("fit_single_model 'verbose' argument is forwarded to INLA::inla()", {
  # Ensure INLA is available
  skip_if_not_installed("INLA")

  single_form <- write_inla_formulas(
    outcome = "dengue_cases",
    covariates = NULL,
    re1 = list(
      id = "month_id",
      model = "rw1",
      cyclic = TRUE
    ),
    baseline = TRUE
  )

  model_quiet <- .fit_single_model(
    formula = single_form,
    data = dengue_MS,
    family = "nbinomial",
    offset_values = dengue_MS$pop100000,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = 2,
    verbose = FALSE
  )

  model_verbose <- .fit_single_model(
    formula = single_form,
    data = dengue_MS,
    family = "nbinomial",
    offset_values = dengue_MS$pop100000,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = 2,
    verbose = TRUE
  )

  # The fitted 'inla' object records the 'verbose' value it was actually
  # called with, so this confirms it reaches INLA::inla() unchanged.
  expect_false(model_quiet$.args$verbose)
  expect_true(model_verbose$.args$verbose)
})

test_that("fit_single_model forwards 'control_inla' and 'control_family' to INLA::inla()", {
  skip_if_not_installed("INLA")

  single_form <- write_inla_formulas(
    outcome = "dengue_cases",
    covariates = NULL,
    re1 = list(
      id = "month_id",
      model = "rw1",
      cyclic = TRUE
    ),
    baseline = TRUE
  )

  # Defaults: adaptive strategy, no family control
  model_default <- .fit_single_model(
    formula = single_form,
    data = dengue_MS,
    family = "nbinomial",
    offset_values = dengue_MS$pop100000,
    nthreads = 2
  )
  expect_equal(model_default$.args$control.inla$strategy, "adaptive")

  # User-supplied values reach INLA unchanged
  model_custom <- .fit_single_model(
    formula = single_form,
    data = dengue_MS,
    family = "nbinomial",
    offset_values = dengue_MS$pop100000,
    control_inla = list(strategy = "gaussian"),
    control_family = list(
      hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01)))
    ),
    nthreads = 2
  )
  expect_s3_class(model_custom, "inla")
  expect_equal(model_custom$.args$control.inla$strategy, "gaussian")
  # INLA stores 'control.family' as a list with one entry per likelihood
  expect_equal(
    as.character(model_custom$.args$control.family[[1]]$hyper$theta$prior),
    "loggamma"
  )
})
