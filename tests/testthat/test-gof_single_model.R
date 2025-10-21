
# test-gof_single_model.R
options(mc.cores = 1)

test_that("gof_single_model extract GOF metrics, when config = FALSE", {
  
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")
  INLA::inla.setOption(num.threads = 1)
  
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
  
  # Build adjacency matrix
  nb <- spdep::poly2nb(map_MS)
  g  <- spdep::nb2mat(nb, style = "B")
  
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
    control_compute = list(config = FALSE),
    nthreads = 4
  )
  
  # Verify we get a valid INLA model object
  
  gof <- .gof_single_model(fitted_model = model,
                          data = dengue_MS, 
                          outcome = "dengue_cases",
                          config = FALSE,
                          ci = FALSE,
                          family = "nbinomial",
                          no_re = TRUE)
  
  # Check the structure of the returned list
  expect_true(is.list(gof), "gof_single_model should return a list")
  expect_true("mod_gof" %in% names(gof), "The returned list should contain 'mod_gof'")
  
  # Check expected columns in gof$mod_gof
  # Typically these columns: dic,  waic, lms, mae, rmse, crps
  expected_cols <- c("dic", 
                     "waic", 
                     "lms",
                     "mae",
                     "rmse",
                     "crps")
  
  expect_true(all(expected_cols %in% colnames(gof$mod_gof)), 
              paste("Missing expected columns in 'mod_gof':",
                    paste(setdiff(expected_cols, colnames(gof$mod_gof)), collapse=", ")))
  
  # Example check: DIC and WAIC should be numeric
  expect_type(gof$mod_gof$dic,  "double")
  expect_type(gof$mod_gof$waic, "double")
  

})


test_that("gof_single_model extract GOF metrics, when CRPS = TRUE", {
  
  # Ensure INLA and spdep are available
  skip_if_not_installed("INLA")
  skip_if_not_installed("spdep")
  INLA::inla.setOption(num.threads = 1)
  
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
  
  # Build adjacency matrix
  nb <- spdep::poly2nb(map_MS)
  g  <- spdep::nb2mat(nb, style = "B")
  
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
    control_compute = list(config = TRUE),
    nthreads = 1
  )
  
  # Verify we get a valid INLA model object
  
  gof <- .gof_single_model(fitted_model = model,
                          data = dengue_MS,
                          outcome = "dengue_cases",
                          config  = TRUE,
                          ci = FALSE,
                          family = "nbinomial",
                          no_re = TRUE)
  
  # Check the structure of the returned list
  expect_true(is.list(gof), "gof_single_model should return a list")
  expect_true("mod_gof" %in% names(gof), "The returned list should contain 'mod_gof'")
  
  # Check expected columns in gof$mod_gof
  # Typically these columns: dic, waic, lms, mae, rmse
  expected_cols <- c("dic", 
                     "waic", 
                     "lms",
                     "mae",
                     "rmse",
                     "crps")
  
  expect_true(all(expected_cols %in% colnames(gof$mod_gof)), 
              paste("Missing expected columns in 'mod_gof':",
                    paste(setdiff(expected_cols, colnames(gof$mod_gof)), collapse=", ")))
  
  # Check dimension of ppd
  expect_true(dim(gof$ppd)[1] == nrow(dengue_MS))
  expect_true(dim(gof$ppd)[2] == 1000)
  
  # Example check: DIC and WAIC should be numeric
  expect_type(gof$mod_gof$dic,  "double")
  expect_type(gof$mod_gof$waic, "double")
  expect_type(gof$mod_gof$lms, "double")
  
})
