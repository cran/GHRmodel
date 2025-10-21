# Test 1: Single time series, group = NULL
test_that("lag_cov works correctly for a single time series without area", {
  data_single <- data.frame(
    time = as.character(as.Date("2023-01-01") + 0:9), # 10 consecutive days
    name1 = 1:10
  )
  result_single <- lag_cov(data = data_single, name = "name1", time = "time", lag = 1)
  
  # Check the number of rows
  expect_equal(nrow(result_single), nrow(data_single))
  
  # Check that the first lagged value is NA
  expect_true(all(is.na(result_single$name1.l1[1])))
  
  # Check functionality for multiple lags
  expect_silent(lag_cov(data = data_single, name = "name1", time = "time", lag = c(1, 3)))
})

# Test 2: Multiple time series with group specified
test_that("lag_cov works correctly for multiple time series with area specified", {
  data_grouped <- data.frame(
    time = rep(as.Date("2023-01-01") + 0:4, 2), # 5 days repeated for 2 areas
    area = rep(c("A", "B"), each = 5),
    name1 = c(1:5, 6:10)
  )
  result_grouped <- lag_cov(data = data_grouped, name = "name1", time = "time", lag = 1, group = "area")
  
  # Check the number of rows
  expect_equal(nrow(result_grouped), nrow(data_grouped))
  
  # Check that areas are preserved
  expect_true(all(result_grouped$area == data_grouped$area))
  
  # Check functionality for multiple lags
  expect_silent(lag_cov(data = data_grouped, name = "name1", time = "time", lag = c(1, 3), group = "area"))
})


# Test 3: Data with a gap in time
test_that("lag_cov returns an error for non-consecutive time series", {
  data_gap <- data.frame(
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-04", "2023-01-05")), # Gap on 2023-01-03
    name1 = 1:4
  )
  # Check that an error is raised
  expect_error(
    lag_cov(data = data_gap, name = "name1", time = "time", lag = 1)
  )
})

# Test 4: Multiple time series without group specified
test_that("lag_cov returns an error for multiple time series without area specified", {
  data_no_area <- data.frame(
    time = rep(as.Date("2023-01-01") + 0:4, 2), # 5 days repeated for 2 areas
    name1 = c(1:5, 6:10)
  )
  # Check that an error is raised
  expect_error(
    lag_cov(data = data_no_area, name = "name1", time = "time", lag = 1)
  )
})

# Test 5: Lag vector with negative values

test_that("lag_cov errors for non‑positive or non‑integer lags", {
  dat <- data.frame(
    time = as.character(as.Date("2023-01-01") + 0:9),
    name1 = 1:10
  )
  
  # single negative lag
  expect_error(
    lag_cov(data = dat, name = "name1", time = "time", lag = -1),
    "'lag' must be a vector of positive integers"
  )
  
  # mixed positive / negative
  expect_error(
    lag_cov(data = dat, name = "name1", time = "time", lag = c(1, -3)),
    "'lag' must be a vector of positive integers"
  )
  
  # non‑integer
  expect_error(
    lag_cov(data = dat, name = "name1", time = "time", lag = c(1, 2.5)),
    "'lag' must be a vector of positive integers"
  )
})

# Test 6: 'name' contains non-existent columns
test_that("lag_cov returns an error when 'name' contains non-existent columns", {
  data <- data.frame(
    time = as.character(as.Date("2023-01-01") + 0:9),
    name1 = 1:10
  )
  
  expect_error(
    lag_cov(data = data, name = c("name1", "name2"), time = "time", lag = 1),
    "The following 'name' or 'time' are not present in the dataset: name2"
  )
})

# Test 9: 'time' column does not exist in data
test_that("lag_cov returns an error when 'time' column is missing", {
  data <- data.frame(
    date = as.character(as.Date("2023-01-01") + 0:9),
    name1 = 1:10
  )
  
  expect_error(
    lag_cov(data = data, name = "name1", time = "time", lag = 1),
    "The following 'name' or 'time' are not present in the dataset: time"
  )
})

# Test 7: Multiple grouping columns
test_that("lag_cov works correctly with multiple grouping columns", {
  data <- data.frame(
    time = rep(as.character(as.Date("2023-01-01") + 0:4), 2),
    region = rep(c("A", "B"), each = 5),
    subregion = rep(c("X", "Y"), each = 5),
    name1 = 1:10
  )
  
  result <- lag_cov(
    data = data,
    name = "name1",
    time = "time",
    lag = 2,
    group = c("region", "subregion")
  )
  
  # Check the number of rows
  expect_equal(nrow(result), nrow(data))
  
  # Check that grouping columns are preserved
  expect_true(all(result$region == data$region))
  expect_true(all(result$subregion == data$subregion))
  
  # Check that the first two lagged values are NA for each group
  expect_true(all(is.na(result$name1.l2[1:2])))
  expect_true(all(is.na(result$name1.l2[6:7])))
})

# Test 8: Duplicate time points within groups
test_that("lag_cov returns an error for duplicate time points within groups", {
  data <- data.frame(
    time = rep(as.character(as.Date("2023-01-01") + 0:4), 2),
    region = rep(c("A", "B"), each = 5),
    name1 = 1:10
  )
  
  # Introduce duplicate time point for region A
  data <- rbind(data, data[3, ])
  
  expect_error(
    lag_cov(data = data, name = "name1", time = "time", lag = 1, group = "region"),
    "Duplicated time units detected. Ensure each group has unique time points."
  )
})


# Test 9: 'add = FALSE' output verification
test_that("lag_cov returns only necessary columns when 'add = FALSE'", {
  data <- data.frame(
    time = as.character(as.Date("2023-01-01") + 0:9),
    name1 = 1:10,
    name2 = 11:20
  )
  
  result <- lag_cov(
    data = data,
    name = c("name1", "name2"),
    time = "time",
    lag = 2,
    add = FALSE
  )
  
  # Expected columns: time, name1.l1, name1.l2, name2.l1, name2.l2
  expected_cols <- c("name1.l2", "name1.l2")
  
  expect_true(all(expected_cols %in% colnames(result)))
  expect_false("name1" %in% colnames(result))
  expect_false("name2" %in% colnames(result))
})


# Test 10: Missing arguments detected

test_that("lag_cov throws an error when required arguments are missing", {
  data <- data.frame(
    time = as.character(as.Date("2023-01-01") + 0:9),
    name1 = 1:10,
    name2 = 11:20
  )
  
  expect_error(lag_cov(), 
               "Missing required argument\\(s\\): data, name, time, lag")
  
  expect_error(lag_cov(data = data), 
               "Missing required argument\\(s\\): name, time, lag")
  
  expect_error(lag_cov(data = data, name = "var"), 
               "Missing required argument\\(s\\): time, lag")
  
  expect_error(lag_cov(data = data, name = "var", time = "date"), 
               "Missing required argument\\(s\\): lag")
})
