# Load example data
data("dengue_MS", package = "GHRmodel")
data("map_MS", package = "GHRmodel")

# Filter data for years after 2015
dengue_MS <- dplyr::filter(dengue_MS, year > 2016)

# Define offset and factor IDs
dengue_MS$offset    <- dengue_MS$population
dengue_MS$month_id  <- as.numeric(as.factor(dengue_MS$month))
dengue_MS$year_id   <- as.numeric(as.factor(dengue_MS$year))
dengue_MS$spat_id   <- as.numeric(as.factor(dengue_MS$micro_code))

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
    formula  = single_form,
    data     = dengue_MS,
    family   = "nbinomial",
    offset   = "population",
    control_compute = list(config = FALSE,
                   vcov = FALSE),
    nthreads = 4
  )
  
  # Verify we get a valid INLA model object
  expect_s3_class(model, "inla")
})
