
  # Load data 
  data("dengue_MS", package = "GHRmodel")
  data("map_MS",  package = "GHRmodel")
  
  # Example: Filter data 
  dengue_MS <- dplyr::filter(dengue_MS, year > 2017)
  
  # Basic transformations
  dengue_MS$spat_id <- as.numeric(as.factor((dengue_MS$micro_code)))
  dengue_MS$spat_meso_id <- as.numeric(as.factor(dengue_MS$meso_code))
  dengue_MS$year_id <- as.numeric(as.factor(dengue_MS$year))
  dengue_MS$year_id2 <- as.numeric(as.factor(dengue_MS$year))
  dengue_MS$month_id <- as.numeric(as.factor(dengue_MS$month))
  dengue_MS$main_climate_f <- as.numeric(as.factor(dengue_MS$main_climate))
  
  # Define multiple predictors
  
  list_cov <- list(
    c("tmin"),
    c("f(INLA::inla.group(tmin, method='quantile', n=10), model='rw2')"),
    c("f(INLA::inla.group(pdsi, method='quantile', n=10), model='rw2', replicate =main_climate_f)"),
    c("tmin",
      "pdsi",
      "urban"),
    c("f(INLA::inla.group(tmin, method='quantile', n=10), model='rw2')",
      "pdsi",
      "urban"),
    c("f(INLA::inla.group(tmin, method='quantile', n=10), model='rw2', replicate =main_climate_f)",
      "pdsi",
      "urban"),
    c("tmin",
      "pdsi",
      "pdsi:tmin"),
    c("f(main_climate_f,tmin, model = 'iid')")
    )

  #Tranform them into inla formulas 
  formulas <- write_inla_formulas(outcome = "dengue_cases",
                                          covariates = list_cov,
                                          re1 = list(id ="month_id", scale = TRUE,
                                                     model ="rw1", cyclic = TRUE),
                                          re2 = list(id = "year_id",
                                                     model = "iid"),
                                          re3 = list(id = "spat_id",
                                                     model = "iid"),
                                          baseline = TRUE)
  
  # Convert to GHRformulas
  ghr_forms <- as_GHRformulas(formulas)
  
  test_that("fit_models works with config = FALSE", {
  
  # Skip if INLA/spdep not installed
  skip_if_not_installed("INLA")

  # Fit multiple models with fit_models
  results <- fit_models(
    formulas = ghr_forms,
    data     = dengue_MS,
    family   = "nbinomial",
    name     = "TestModel",
    offset   = "population",
    nthreads = 2,
    control_compute = list(config = FALSE),
    pb       = TRUE
  )
  
  # Basic checks on returned object
  expect_s3_class(results, "GHRmodels")
  expect_true(is.list(results), "Returned object should be a list")
  
  # Check mod_gof structure
  expect_true("mod_gof" %in% names(results), "Should contain 'mod_gof'")
  # Typical columns: dic, waic, lms, rsq
  gof_cols <- c("dic", "waic", "lms", "r2dev", "mae", "rmse", "crps")
  for (col in gof_cols) {
    expect_true(col %in% colnames(results$mod_gof), paste("Column missing in 'mod_gof':", col))
  }
  
  # Ensure we have as many model results as formulas
  n_forms <- length(formulas)
  
  expect_length(results$fitted,  n_forms)
  expect_length(results$fixed,   n_forms)
  expect_length(results$random,  n_forms)
  expect_length(results$formulas, n_forms)
  
})
  
  
  
test_that("fit_models works with config = TRUE", {
    
  # Skip if INLA/spdep not installed
  skip_if_not_installed("INLA")

  # Fit multiple models with fit_models
  results <- fit_models(
  formulas = ghr_forms,
  data     = dengue_MS,
  family   = "nbinomial",
  name     = "TestModel",
  offset   = "population",
  nthreads = 2,
  control_compute = list(config = TRUE),
  pb       = TRUE
  )
    
  # Basic checks on returned object
  expect_s3_class(results, "GHRmodels")
  expect_true(is.list(results), "Returned object should be a list")
    
  # Check mod_gof structure
  expect_true("mod_gof" %in% names(results), "Should contain 'mod_gof'")
  # Typical columns: dic, waic, lms, rsq, mae, rmse, crps
  gof_cols <- c("dic", "waic", "lms", "r2dev", "mae", "rmse", "crps")
  for (col in gof_cols) {
   expect_true(col %in% colnames(results$mod_gof), paste("Column missing in 'mod_gof':", col))
   }
    
  # Ensure we have as many model results as formulas
  n_forms <- length(formulas)
    
  expect_length(results$fitted,  n_forms)
  expect_length(results$fixed,   n_forms)
  expect_length(results$random,  n_forms)
  expect_length(results$formulas, n_forms)
    
  })