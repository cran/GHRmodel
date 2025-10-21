# Sample dataset for testing
test_data <- data.frame(
  temp_lag1 = rnorm(10),
  temp_lag2 = rnorm(10),
  humidity_lag1 = rnorm(10),
  wind_lag1 = rnorm(10),
  region = rep(c("A", "B"), each = 5)
)



### TESTS FOR extract_names() ----
test_that("extract_names() returns expected covariates", {
  test_data <- data.frame(
    temp = rnorm(10),
    temp_lag1 = rnorm(10),
    temp_lag2 = rnorm(10),
    humidity_lag1 = rnorm(10),
    wind_lag1 = rnorm(10),
    region = rep(c("A", "B"), each = 5)
  )
  result <- extract_names(test_data, pattern = c("temp_l",  "humidity"), name= "temp")
  expect_true(all(result %in% names(test_data)))  # All elements should be valid column names
  
  # Check content
  expected_vars <- c("temp", "temp_lag1", "temp_lag2", "humidity_lag1")
  expect_setequal(result, expected_vars)
})

test_that("extract_names() empty", {
  expect_error(extract_names(test_data, pattern = c("invalid_var")), 
                                 "No matching variables found in the dataset.")  # Should return error
})

### TESTS FOR cov_uni() ----

test_that("cov_uni selects covariates by pattern", {
  covs <- c("temp1", "temp2", "rain", "wind")
  result <- cov_uni(covariates = covs, pattern = "temp")
  expect_equal(result, list("temp1", "temp2"))
})

test_that("cov_uni selects covariates by exact name", {
  covs <- c("temp1", "temp2", "rain", "wind")
  result <- cov_uni(covariates = covs, name = c("rain", "wind"))
  expect_equal(result, list("rain", "wind"))
})

test_that("cov_uni handles combined pattern and name", {
  covs <- c("temp1", "temp2", "rain", "wind")
  result <- cov_uni(covariates = covs, pattern = "temp", name = "rain")
  expect_equal(result, list("temp1", "temp2", "rain"))
})

test_that("cov_uni errors if no pattern or name is provided", {
  covs <- c("temp1", "temp2", "rain", "wind")
  expect_error(cov_uni(covariates = covs), 
               "At least one of 'pattern' or 'name' must be provided.")
})

test_that("cov_uni errors if covariates is not character", {
  expect_error(cov_uni(covariates = 1:5, pattern = "x"), 
               "'covariates' must be a character vector.")
})

test_that("cov_uni errors if pattern is not character", {
  covs <- c("x1", "x2")
  expect_error(cov_uni(covariates = covs, pattern = 123), 
               "'pattern' must be a character vector.")
})

test_that("cov_uni errors if name is not character", {
  covs <- c("x1", "x2")
  expect_error(cov_uni(covariates = covs, name = 123), 
               "'name' must be a character vector.")
})

test_that("cov_uni errors when a pattern does not match anything", {
  covs <- c("x1", "x2")
  expect_error(cov_uni(covariates = covs, pattern = "y"), 
               "No matches found for: y")
})

test_that("cov_uni errors when a name does not match anything", {
  covs <- c("x1", "x2")
  expect_error(cov_uni(covariates = covs, name = "y"), 
               "No matches found for: y")
})

test_that("cov_uni errors when mixed valid and invalid names/patterns", {
  covs <- c("x1", "x2", "temp1")
  expect_error(cov_uni(covariates = covs, pattern = c("temp", "rain")), 
               "No matches found for: rain")
  expect_error(cov_uni(covariates = covs, name = c("x1", "x3")), 
               "No matches found for: x3")
})


### TESTS FOR cov_multi() ----
test_that("cov_multi() returns correct combinations", {
  test_data <- data.frame(
    temp = rnorm(10),
    temp_lag1 = rnorm(10),
    temp_lag2 = rnorm(10),
    humidity = rnorm(10),
    humidity_lag1 = rnorm(10),
    humidity_lag2 = rnorm(10),
    wind_lag1 = rnorm(10),
    region = rep(c("A", "B"), each = 5)
  )
  single_cov <- extract_names(test_data, pattern = c("temp_lag", "humidity"), name= c("temp"))
  
  result <- cov_multi(single_cov, pattern = c("temp_lag", "humidity"), name= c("temp"))
  
  expect_type(result, "list")  # Should return a list
  expect_true(all(sapply(result, is.character)))  # Each combination should be a character vector
  
  # Verify content: Expected combinations
  expected_combinations <- list(
    c("temp_lag1", "humidity_lag1","temp" ),
    c("temp_lag2", "humidity_lag1", "temp")
  )
  expect_true(any(sapply(result, function(x) setequal(x, expected_combinations[[1]]))))
  expect_true(any(sapply(result, function(x) setequal(x, expected_combinations[[2]]))))
})

test_that("cov_multi() returns error when wrong object is provided", {
    expect_error(cov_multi(test_data, pattern = c("invalid_var","temp")), 
                 "If 'covariates' is a list, all elements must be single-element character vectors.") 
})


### TESTS FOR cov_interact() ----
sample_covariates_list <- list(c("temp_lag1", "humidity", "pressure", "rainfall"))
sample_covariates_vec <- c("temp_lag1", "humidity", "pressure", "rainfall")

test_that("cov_interact() correctly generates interactions for two variables", {
  result_list <- cov_interact(sample_covariates_list, pattern = c("temp", "humidity"))
  
  expect_type(result_list, "list")
  expect_true(all(sapply(result_list, is.character)))  # Ensure output is character
  expected_interaction <- "temp_lag1:humidity"
  expect_true(expected_interaction %in% result_list[[1]])  # Interaction should be present
  
  result_vec <- cov_interact(sample_covariates_list, pattern = c("temp", "humidity"))
  expect_type(result_vec, "list")
  expect_true(all(sapply(result_vec, is.character)))  # Ensure output is character
  expected_interaction <- "temp_lag1:humidity"
  expect_true(expected_interaction %in% result_vec[[1]])  # Interaction should be present
  
  result_list_added <-  cov_interact(sample_covariates_list, pattern = c("temp", "humidity"), 
                                               add = TRUE)
  expect_equal(length(result_list_added), 2)
  
})

test_that("cov_interact() correctly generates interactions for three variables", {
  result <- cov_interact(sample_covariates_list, pattern = c("temp", "humidity", "rainfall"))
  
  expect_type(result, "list")
  expect_true(all(sapply(result, is.character)))  # Ensure output is character
  
  # Expected interactions
  expected_interactions <- c(
    "temp_lag1:rainfall",
    "temp_lag1:humidity",
    "humidity:rainfall",
    "temp_lag1:humidity:rainfall"
  )
  
  expect_true(all(expected_interactions %in% result[[1]]))  # Ensure all interactions are present
})

### TESTS FOR cov_nl() ----

sample_covariates <- list("temp", "humidity", "pressure", "rainfall")
test_that("cov_nl() correctly applies transformation", {
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw2"
  )
  
  expect_type(result, "list")  # Now always returns a list
  expect_length(result, length(sample_covariates))  # Matches number of input vectors
  
  # Verify exact transformation output for humidity
  expected_transformation <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  expect_true(any(sapply(result, function(x) expected_transformation %in% x)))
  
  # With add
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw2", add = TRUE
  )
  expect_type(result, "list")
  expect_equal(length(result), 5L)  # 3 original + 2 transformed (if unique)
})

test_that("cov_nl() correctly applies transformation to all matching patterns", {
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = c("humidity", "temp"),
    model = "rw1"
  )
  
  expected_temp <- "f(INLA::inla.group(temp, method='quantile', n=10), model='rw1')"
  expected_humidity <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw1')"
  
  expect_true(any(sapply(result, function(x) expected_temp %in% x)))
  expect_true(any(sapply(result, function(x) expected_humidity %in% x)))
})

test_that("cov_nl() does not transform unmatched covariates", {
  expect_error(cov_nl(covariates = sample_covariates, pattern = "wind", model = "rw2"), 
               "No matches found for: wind") 
})

test_that("cov_nl() supports different discretization methods", {
  result_cut <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    method = "cut"
  )
  
  result_quantile <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    method = "quantile"
  )
  
  expected_cut <- "f(INLA::inla.group(humidity, method='cut', n=10), model='rw2')"
  expected_quantile <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  
  expect_true(any(sapply(result_cut, function(x) expected_cut %in% x)))
  expect_true(any(sapply(result_quantile, function(x) expected_quantile %in% x)))
})

test_that("cov_nl() correctly applies different models", {
  result_rw1 <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw1"
  )
  
  result_rw2 <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw2"
  )
  
  expected_rw1 <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw1')"
  expected_rw2 <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  
  expect_true(any(sapply(result_rw1, function(x) expected_rw1 %in% x)))
  expect_true(any(sapply(result_rw2, function(x) expected_rw2 %in% x)))
})

test_that("cov_nl() correctly handles custom n values", {
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    n = 20
  )
  
  expected_transformation <- "f(INLA::inla.group(humidity, method='quantile', n=20), model='rw2')"
  expect_true(any(sapply(result, function(x) expected_transformation %in% x)))
})

test_that("cov_nl() correctly handles replicate parameter", {
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    replicate = "region_id"
  )
  
  expected_transformation <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2', replicate=region_id)"
  expect_true(any(sapply(result, function(x) expected_transformation %in% x)))
})

### TEST CASE: multiple variable NON-Linear Transformations

sample_covariates_list <- list(
  c("temp", "humidity", "pressure"),
  c("rainfall", "wind", "solar")
)

test_that("cov_nl() correctly applies transformation for two variables in a list", {
  result <- cov_nl(
    covariates = sample_covariates_list,
    pattern = c("humidity", "temp"),
    model = "rw2"
  )
  
  expect_type(result, "list")
  expect_true(all(sapply(result, is.character)))
  
  expected_humidity <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  expected_temp <- "f(INLA::inla.group(temp, method='quantile', n=10), model='rw2')"
  
  expect_true(expected_humidity %in% result[[1]])
  expect_true(expected_temp %in% result[[1]])
  
  expect_identical(result[[2]], sample_covariates_list[[2]])
  
  # With add
  result <- cov_nl(
    covariates = sample_covariates_list,
    pattern = c("humidity", "temp"),
    model = "rw2", add = TRUE
  )
  expect_equal(length(result), 3L)  # 2 original + 1 transformed (unique)
})

### TEST CASE: vector NON-Linear Transformations

sample_covariates_vector <- c("temp", "humidity", "pressure")

test_that("cov_nl() correctly applies transformation for two variables in a list", {
  result <- cov_nl(
    covariates = sample_covariates_vector,
    pattern = c("humidity", "temp"),
    model = "rw2"
  )
  
  expect_type(result, "list")
  expect_true(all(sapply(result, is.character)))
  
  expected_humidity <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  expected_temp <- "f(INLA::inla.group(temp, method='quantile', n=10), model='rw2')"
  
  expect_true(expected_temp %in% result[[1]])
  expect_true(expected_humidity %in% result[[2]])

  
  # With add
  result <- cov_nl(
    covariates = sample_covariates_vector,
    pattern = c("humidity", "temp"),
    model = "rw2", add = TRUE
  )
  expect_equal(length(result), 5L)  # 2 original + 1 transformed (unique)
})

### TEST cov_varying() ---- 

sample_covariates_list <- list(
  c("temp", "humidity", "pressure"),
  c("rainfall", "wind", "solar")
)

test_that("cov_varying() correctly applies transformation", {
  result <- cov_varying(
    covariates = sample_covariates_list,
    pattern = c("temp"),
    model = "iid", unit = "spat_id"
  )
  
  expect_type(result, "list")  # Output should be a list
  expect_true(all(sapply(result, is.character)))  # Ensure each list item is character
  
  # Expected transformations in the first vector
  expected_temp <- "f(spat_id, temp, model = 'iid', constr = FALSE)"
  expect_true(any(sapply(result[[1]], function(x) x == expected_temp)))
  
  # Ensure that the second vector remains unchanged (no matching variables)
  expect_identical(result[[2]], sample_covariates_list[[2]])
  
  # With add
  result <- cov_varying(
    covariates = sample_covariates_list,
    pattern = c("temp"),
    model = "iid", unit = "spat_id", add=TRUE
  )
  expect_identical(length(result), 3L)
  
})



### TEST GET COVARIATES ----

test_that("get_covariates returns expected covariates", {
  # Minimal mock GHRmodels object
  mock_model <- list(
    mod_gof = data.frame(
      covariate1 = c("tmin", "tmin_nl_q10_rw2"),
      covariate2 = c("pdsi_nl_q10_rw2_rep_main_climate_f", NA),
      stringsAsFactors = FALSE
    )
  )
  class(mock_model) <- c("GHRmodels", "list")
  
  # Expected values if .join_inla_group strips _inla_group_* suffix
  expected_combined <- list(
    c("tmin", "f(INLA::inla.group(pdsi, method = 'quantile', n = 10), model = 'rw2', replicate = main_climate_f)"),
    c("f(INLA::inla.group(tmin, method = 'quantile', n = 10), model = 'rw2')")
  )
  expected_unique <- sort(unique(unlist(expected_combined)))
  
  # Call function with unique = FALSE
  result_combined <- get_covariates(mock_model, unique = FALSE)
  expect_type(result_combined, "list")
  expect_equal(result_combined, expected_combined)
  
  # Call function with unique = TRUE
  result_unique <- get_covariates(mock_model, unique = TRUE)
  expect_type(result_unique, "list")
  expect_equal(sort(unlist(result_unique)), expected_unique)
})

test_that("get_covariates throws error on wrong input class", {
  expect_error(get_covariates(list(a = 1)), "'model' must be an object of class 'GHRmodels'")
})

