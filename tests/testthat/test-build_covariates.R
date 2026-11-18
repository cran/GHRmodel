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
  result <- extract_names(
    test_data,
    pattern = c("temp_l", "humidity"),
    name = "temp"
  )
  # All elements should be valid column names
  expect_true(all(result %in% names(test_data)))

  # Check content
  expected_vars <- c("temp", "temp_lag1", "temp_lag2", "humidity_lag1")
  expect_setequal(result, expected_vars)
})

test_that("extract_names() empty", {
  expect_error(
    extract_names(test_data, pattern = c("invalid_var")),
    "No matching variables found in the dataset."
  ) # Should return error
})

test_that("extract_names() errors if data is missing or not a data.frame", {
  expect_error(
    extract_names(pattern = "tmin"),
    "The 'data' argument must be provided."
  )
  expect_error(
    extract_names(data = "not_a_df", pattern = "tmin"),
    "'data' must be a data frame."
  )
})

test_that("extract_names() pattern matching is case-sensitive", {
  case_data <- data.frame(TMIN = 1:3, urban = 1:3)
  expect_error(
    extract_names(case_data, pattern = "tmin"),
    "No matching variables found in the dataset."
  )
  expect_equal(extract_names(case_data, pattern = "TMIN"), "TMIN")
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
  expect_error(
    cov_uni(covariates = covs),
    "At least one of 'pattern' or 'name' must be provided."
  )
})

test_that("cov_uni errors if covariates is not character", {
  expect_error(
    cov_uni(covariates = 1:5, pattern = "x"),
    "'covariates' must be a character vector."
  )
})

test_that("cov_uni errors if pattern is not character", {
  covs <- c("x1", "x2")
  expect_error(
    cov_uni(covariates = covs, pattern = 123),
    "'pattern' must be a character vector."
  )
})

test_that("cov_uni errors if name is not character", {
  covs <- c("x1", "x2")
  expect_error(
    cov_uni(covariates = covs, name = 123),
    "'name' must be a character vector."
  )
})

test_that("cov_uni errors when a pattern does not match anything", {
  covs <- c("x1", "x2")
  expect_error(
    cov_uni(covariates = covs, pattern = "y"),
    "No matches found for: y"
  )
})

test_that("cov_uni errors when a name does not match anything", {
  covs <- c("x1", "x2")
  expect_error(
    cov_uni(covariates = covs, name = "y"),
    "No matches found for: y"
  )
})

test_that("cov_uni errors when mixed valid and invalid names/patterns", {
  covs <- c("x1", "x2", "temp1")
  expect_error(
    cov_uni(covariates = covs, pattern = c("temp", "rain")),
    "No matches found for: rain"
  )
  expect_error(
    cov_uni(covariates = covs, name = c("x1", "x3")),
    "No matches found for: x3"
  )
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
  single_cov <- extract_names(
    test_data,
    pattern = c("temp_lag", "humidity"),
    name = c("temp")
  )

  result <- cov_multi(
    single_cov,
    pattern = c("temp_lag", "humidity"),
    name = c("temp")
  )

  expect_type(result, "list") # Should return a list
  # Each combination should be a character vector
  expect_true(all(sapply(result, is.character)))

  # Verify content: Expected combinations
  expected_combinations <- list(
    c("temp_lag1", "humidity_lag1", "temp"),
    c("temp_lag2", "humidity_lag1", "temp")
  )
  expect_true(any(sapply(result, function(x) {
    setequal(x, expected_combinations[[1]])
  })))
  expect_true(any(sapply(result, function(x) {
    setequal(x, expected_combinations[[2]])
  })))
})

test_that("cov_multi() returns error when wrong object is provided", {
  expect_error(
    cov_multi(test_data, pattern = c("invalid_var", "temp")),
    "If 'covariates' is a list, all elements must be "
  )
})

test_that("cov_multi() errors when covariates is NULL", {
  expect_error(
    cov_multi(covariates = NULL, pattern = "tmin"),
    "'covariates' must be provided."
  )
})

test_that("cov_multi() returns one combo per covariate for a single group", {
  result <- cov_multi(covariates = c("tmin", "tmin.l1"), pattern = "tmin")
  expect_equal(length(result), 2L)
  expect_true(all(sapply(result, length) == 1L))
  expect_setequal(unlist(result), c("tmin", "tmin.l1"))
})

test_that("cov_multi() pattern matching is regex-based, not prefix-only", {
  # documented as regex: matches the pattern anywhere in the string
  result <- cov_multi(covariates = c("tmin", "wintmin"), pattern = "tmin")
  expect_setequal(unlist(result), c("tmin", "wintmin"))
})

test_that("cov_multi() add = TRUE appends the full covariate set", {
  result <- cov_multi(
    covariates = c("tmin", "tmin.l1", "pdsi"),
    pattern = "tmin",
    add = TRUE
  )
  expect_equal(length(result), 3L) # 2 single-covariate combos + 1 full set
  expect_true(any(sapply(result, function(x) {
    setequal(x, c("tmin", "tmin.l1", "pdsi"))
  })))
})


### TESTS FOR cov_interact() ----
sample_covariates_list <- list(c(
  "temp_lag1",
  "humidity",
  "pressure",
  "rainfall"
))
sample_covariates_vec <- c("temp_lag1", "humidity", "pressure", "rainfall")

test_that("cov_interact() correctly generates interactions for two variables", {
  result_list <- cov_interact(
    sample_covariates_list,
    pattern = c("temp", "humidity")
  )

  expect_type(result_list, "list")
  expect_true(all(sapply(result_list, is.character))) # Ensure output is char
  expected_interaction <- "temp_lag1:humidity"
  expect_true(expected_interaction %in% result_list[[1]]) # Interaction check

  result_vec <- cov_interact(
    sample_covariates_list,
    pattern = c("temp", "humidity")
  )
  expect_type(result_vec, "list")
  expect_true(all(sapply(result_vec, is.character))) # Ensure output is char
  expected_interaction <- "temp_lag1:humidity"
  expect_true(expected_interaction %in% result_vec[[1]]) # Interaction check

  result_list_added <- cov_interact(
    sample_covariates_list,
    pattern = c("temp", "humidity"),
    add = TRUE
  )
  expect_equal(length(result_list_added), 2)
})

test_that("cov_interact() generates interactions for three variables", {
  result <- cov_interact(
    sample_covariates_list,
    pattern = c("temp", "humidity", "rainfall")
  )

  expect_type(result, "list")
  expect_true(all(sapply(result, is.character))) # Ensure output is character

  # Expected interactions
  expected_interactions <- c(
    "temp_lag1:rainfall",
    "temp_lag1:humidity",
    "humidity:rainfall",
    "temp_lag1:humidity:rainfall"
  )

  expect_true(all(expected_interactions %in% result[[1]])) # Interaction check
})

test_that("cov_interact() rejects an interacting variable that is non-linear", {
  nl_term <- "f(INLA::inla.group(tmin, method='quantile', n=10), model='rw2')"
  expect_error(
    cov_interact(list(c(nl_term, "humidity")), name = c(nl_term, "humidity")),
    "non-linear and not supported in interactions"
  )
})

test_that("cov_interact() does not reject unrelated non-linear covariates", {
  nl_term <- "f(INLA::inla.group(pdsi, method='quantile', n=10), model='rw2')"
  covs_list <- list(
    c("tmin", "humidity"),
    c(nl_term, "wind")
  )
  result <- cov_interact(covs_list, pattern = c("tmin", "humidity"))
  expect_true("tmin:humidity" %in% result[[1]])
  expect_identical(result[[2]], covs_list[[2]])
})

test_that("cov_interact() leaves a set unchanged when 4+ variables match", {
  covs_list <- list(c("tmin", "humidity", "pressure", "wind"))
  result <- cov_interact(
    covs_list,
    pattern = c("tmin", "humidity", "pressure", "wind")
  )
  expect_identical(result[[1]], covs_list[[1]])
})

test_that("cov_interact() add = TRUE dedup is order-insensitive", {
  covs_list <- list(c("tmin", "humidity"), c("humidity", "tmin"))
  result <- cov_interact(
    covs_list,
    pattern = c("tmin", "humidity"),
    add = TRUE
  )
  # 1 deduped original + 2 distinct interaction terms
  expect_equal(length(result), 3L)
  expect_true(any(sapply(result, function(x) {
    setequal(x, c("tmin", "humidity"))
  })))
  expect_true(any(sapply(result, function(x) "tmin:humidity" %in% x)))
  expect_true(any(sapply(result, function(x) "humidity:tmin" %in% x)))
})

### TESTS FOR cov_nl() ----

sample_covariates <- list("temp", "humidity", "pressure", "rainfall")
test_that("cov_nl() correctly applies transformation", {
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw2"
  )

  expect_type(result, "list") # Now always returns a list
  # Matches number of input vectors
  expect_length(result, length(sample_covariates))

  # Verify exact transformation output for humidity
  expected_transformation <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  expect_true(any(sapply(result, function(x) expected_transformation %in% x)))

  # With add
  result <- cov_nl(
    covariates = sample_covariates,
    pattern = "humidity",
    model = "rw2",
    add = TRUE
  )
  expect_type(result, "list")
  expect_equal(length(result), 5L) # 3 original + 2 transformed (if unique)
})

test_that("cov_nl() applies transformation to all matching patterns", {
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
  expect_error(
    cov_nl(covariates = sample_covariates, pattern = "wind", model = "rw2"),
    "No matches found for: wind"
  )
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
  expect_true(any(sapply(result_quantile, function(x) {
    expected_quantile %in% x
  })))
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
    model = "rw2",
    add = TRUE
  )
  expect_equal(length(result), 3L) # 2 original + 1 transformed (unique)
})

### TEST CASE: vector NON-Linear Transformations

sample_covariates_vector <- c("temp", "humidity", "pressure")

test_that("cov_nl() applies transformation for two variables in a list", {
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
    model = "rw2",
    add = TRUE
  )
  expect_equal(length(result), 5L) # 2 original + 1 transformed (unique)
})

test_that("cov_nl() transforms covariates selected via 'name'", {
  result <- cov_nl(
    covariates = c("tmin", "humidity"),
    name = "humidity",
    model = "rw2",
    n = 10
  )
  expect_type(result, "list")
  expected_transformation <- "f(INLA::inla.group(humidity, method='quantile', n=10), model='rw2')"
  expect_true(expected_transformation %in% unlist(result))
  expect_true("tmin" %in% unlist(result))
})

test_that("cov_nl() errors on invalid model/method/n/covariates", {
  expect_error(
    cov_nl(covariates = c("tmin"), pattern = "tmin", model = "bad"),
    "'model' must be either 'rw1' or 'rw2'."
  )
  expect_error(
    cov_nl(covariates = c("tmin"), pattern = "tmin", method = "bad"),
    "'method' must be either 'cut' or 'quantile'."
  )
  expect_error(
    cov_nl(covariates = c("tmin"), pattern = "tmin", n = 1),
    "'n' must be an integer >= 2."
  )
  expect_error(
    cov_nl(pattern = "tmin"),
    "'covariates' must be provided."
  )
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
    model = "iid",
    unit = "spat_id"
  )

  expect_type(result, "list") # Output should be a list
  expect_true(all(sapply(result, is.character))) # Ensure each list item is char

  # Expected transformations in the first vector
  expected_temp <- "f(spat_id, temp, model = 'iid', constr = FALSE)"
  expect_true(any(sapply(result[[1]], function(x) x == expected_temp)))

  # Ensure that the second vector remains unchanged (no matching variables)
  expect_identical(result[[2]], sample_covariates_list[[2]])

  # With add
  result <- cov_varying(
    covariates = sample_covariates_list,
    pattern = c("temp"),
    model = "iid",
    unit = "spat_id",
    add = TRUE
  )
  expect_identical(length(result), 3L)
})

test_that("cov_varying() transforms covariates selected via 'name'", {
  result <- cov_varying(
    covariates = c("tmin", "humidity"),
    name = "humidity",
    unit = "spat_id"
  )
  expect_true(any(sapply(result, function(x) {
    "f(spat_id, humidity, model = 'iid', constr = FALSE)" %in% x
  })))
})

test_that("cov_varying() errors when 'unit' is missing or invalid", {
  expect_error(
    cov_varying(covariates = c("tmin"), pattern = "tmin"),
    "'unit' must be a single character string."
  )
})

test_that("cov_varying() errors on an unsupported model", {
  expect_error(
    cov_varying(
      covariates = c("tmin"),
      pattern = "tmin",
      unit = "spat_id",
      model = "rw1"
    ),
    "'model' must be 'iid'. Other models are not yet supported."
  )
})

test_that("cov_varying() errors when covariates is missing", {
  expect_error(
    cov_varying(unit = "spat_id", pattern = "tmin"),
    "'covariates' must be provided."
  )
})

test_that("cov_varying() forwards constr = TRUE to the generated term", {
  result <- cov_varying(
    covariates = c("tmin"),
    pattern = "tmin",
    unit = "spat_id",
    constr = TRUE
  )
  expect_true(grepl("constr = TRUE", result[[1]]))
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
    c(
      "tmin",
      "f(INLA::inla.group(pdsi, method = 'quantile', n = 10), model = 'rw2', replicate = main_climate_f)"
    ),
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
  expect_error(
    get_covariates(list(a = 1)),
    "'model' must be an object of class 'GHRmodels'"
  )
})

test_that("get_covariates handles a single-row mod_gof", {
  mock_model <- list(
    mod_gof = data.frame(
      covariate1 = "tmin",
      covariate2 = "pdsi_nl_q10_rw2",
      stringsAsFactors = FALSE
    )
  )
  class(mock_model) <- c("GHRmodels", "list")

  result <- get_covariates(mock_model, unique = FALSE)
  expected <- list(c(
    "tmin",
    "f(INLA::inla.group(pdsi, method = 'quantile', n = 10), model = 'rw2')"
  ))
  expect_equal(result, expected)
})


### TEST cov_add() ----

test_that("cov_add() appends a name to every covariate set", {
  cov_sets <- list(
    c("tmin", "pdsi"),
    c("tmin.l1", "pdsi"),
    c("tmin.l2", "pdsi")
  )
  result <- cov_add(cov_sets, name = "urban")
  expect_type(result, "list")
  expect_equal(length(result), length(cov_sets))
  expect_true(all(sapply(result, function(x) "urban" %in% x)))
})

test_that("cov_add() accepts a single character vector", {
  result <- cov_add(c("tmin", "pdsi"), name = "urban")
  expect_equal(length(result), 1L)
  expect_setequal(result[[1]], c("tmin", "pdsi", "urban"))
})

test_that("cov_add() de-duplicates when a covariate is added twice", {
  result <- cov_add(c("tmin", "urban"), name = "urban")
  expect_setequal(result[[1]], c("tmin", "urban"))
})

test_that("cov_add() add = TRUE keeps the original combinations", {
  cov_sets <- list(c("tmin", "pdsi"), c("tmin.l1", "pdsi"))
  result <- cov_add(cov_sets, name = "urban", add = TRUE)
  expect_equal(length(result), 4L) # 2 original + 2 with 'urban' added
  expect_true(any(sapply(result, function(x) setequal(x, c("tmin", "pdsi")))))
  expect_true(any(sapply(result, function(x) {
    setequal(x, c("tmin", "pdsi", "urban"))
  })))
})

test_that("cov_add() errors on invalid arguments", {
  expect_error(
    cov_add(covariates = NULL, name = "urban"),
    "'covariates' must be a character vector or a list of character vectors."
  )
  expect_error(
    cov_add(covariates = c("tmin"), name = 1),
    "'name' must be a character vector."
  )
  expect_error(
    cov_add(covariates = c("tmin"), name = "urban", add = "yes"),
    "'add' must be a single logical value"
  )
})
