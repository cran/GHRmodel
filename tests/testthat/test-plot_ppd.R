test_that("plot_ppd tests", {
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")

  # Load example dataset
  data(dengue_MS)
  dat <- dengue_MS[dengue_MS$year %in% 2015:2016, ]
  # Declare formulas
  formulas <- c("dengue_cases ~ tmin +  f(year, model='rw1')")
  # Tranform formulas into a 'GHRformulas' object
  ghr_formula <- as_GHRformulas(formulas)
  # Fit multiple models
  results <- fit_models(
    formulas = ghr_formula,
    data = dat,
    family = "nbinomial",
    name = "model",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = TRUE),
    keep_ppd = TRUE,
    # A small sample count is enough here: this test only checks the
    # returned object is a valid ggplot, not the plotted density's shape,
    # and plot_ppd() computes an expensive 4096-point KDE per ppd column.
    ppd_samples = 15
  )

  # Mismatched obs
  expect_error(
    plot_ppd(results$ppd[[1]], obs = dat$dengue_cases[1:10]),
    "must have the same length"
  )

  # Plot densities of the posterior predictive distribution and observed cases.
  p <- plot_ppd(results$ppd[[1]], obs = dat$dengue_cases)
  expect_s3_class(p, "ggplot")

  # Valid axes
  expect_error(
    plot_ppd(results$ppd[[1]], obs = dat$dengue_cases, xlim = "invalid"),
    "`xlim` must be a numeric vector of length 2"
  )
  expect_error(
    plot_ppd(results$ppd[[1]], obs = dat$dengue_cases, xlim = c(1)),
    "`xlim` must be a numeric vector of length 2"
  )
})
