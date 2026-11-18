# Tests for select_fe().
#
# Fitting through INLA is by far the most expensive part of select_fe()
# (one model per univariable term, then one per multivariable combination),
# so every test in this file mocks 'fit_models()' with a fast, deterministic
# fake. The real select_fe() logic (input validation, rolling statistics,
# lags, standardization, formula generation, ranking and selection) runs
# end-to-end in milliseconds, and no test needs INLA installed.
#
# The fake returns a 'mod_gof' table built from the covariate columns that
# 'as_GHRformulas()' extracts from the formulas, and it makes the baseline
# (random effects only) model the *best* one on every metric. That is
# deliberate: it turns "the baseline leaks into the ranking/selection" into an
# observable failure instead of a silent one.

# ---- Shared fixtures ----

n_units <- 2
n_days <- 12
set.seed(1)
dat <- data.frame(
  unit = rep(c("u1", "u2"), each = n_days),
  date = rep(as.Date("2020-01-01") + 0:(n_days - 1), times = n_units),
  seasonal_id = rep(seq_len(n_days), times = n_units),
  year_id = rep(seq_len(n_units), each = n_days),
  spatial_id = rep(seq_len(n_units), each = n_days),
  pop = 1000,
  tmax = stats::rnorm(n_units * n_days, 25, 3),
  pdsi = stats::rnorm(n_units * n_days),
  y = stats::rpois(n_units * n_days, 5)
)

RE <- "f(seasonal_id, model = 'rw1', cyclic = TRUE)"

# Fake fit_models(): records every call in 'cap' and returns a minimal
# GHRmodels-like object. Terms containing 'prefer' get a lower (better) WAIC;
# the baseline gets the lowest of all.
make_fake_fit_models <- function(cap, prefer = "rmean3") {
  function(
    data,
    formulas,
    name,
    family,
    offset_var,
    control_compute,
    nthreads,
    ...
  ) {
    cap$calls[[length(cap$calls) + 1]] <- list(
      data = data,
      formulas = formulas,
      family = family,
      offset_var = offset_var,
      nthreads = nthreads
    )
    vars <- formulas$vars
    n <- nrow(vars)
    terms <- do.call(paste, c(vars, sep = " "))
    waic <- 200 + seq_len(n) - 20 * grepl(prefer, terms)
    waic[rowSums(!is.na(vars)) == 0] <- 0
    structure(
      list(
        mod_gof = data.frame(
          model_id = paste0(name, seq_len(n)),
          vars,
          waic = waic,
          lms = waic / 100,
          re_meanchange = 0
        )
      ),
      class = "GHRmodels"
    )
  }
}

# Runs the real select_fe() with the fake fit_models() and sensible defaults.
# Any argument can be overridden (or set to NULL) through '...'.
run_fe <- function(cap = new.env(), ..., .quiet = TRUE) {
  testthat::local_mocked_bindings(
    fit_models = make_fake_fit_models(cap),
    .package = "GHRmodel"
  )
  args <- list(
    data = dat,
    outcome = "y",
    covariates = c("tmax", "pdsi"),
    time = "date",
    unit = "unit",
    re_mod = RE,
    metric = "waic"
  )
  extra <- list(...)
  args[names(extra)] <- extra
  if (.quiet) {
    suppressMessages(do.call(select_fe, args))
  } else {
    do.call(select_fe, args)
  }
}

# A minimal stand-in for the output of select_re().
fake_select_re <- function() {
  structure(
    list(
      re_multi_gof = data.frame(
        model_id = c("re1", "re2"),
        effects = c(RE, "f(year_id, model = 'iid')")
      ),
      best = c(re1 = RE)
    ),
    class = "GHRselect_re"
  )
}


### 0. Input validation ----

test_that("select_fe errors on missing data columns", {
  expect_error(run_fe(outcome = "nope"), "'outcome' not found")
  expect_error(
    run_fe(covariates = c("tmax", "nope")),
    "Covariate nope not found"
  )
  expect_error(run_fe(offset_var = "nope"), "'offset_var' not found")
  expect_error(
    run_fe(time = "nope"),
    "No column of the data matches the 'time'"
  )
  expect_error(
    run_fe(unit = "nope"),
    "No column of the data matches the 'unit'"
  )
})

test_that("select_fe errors on missing values in time, unit and covariates", {
  bad <- dat
  bad$date[3] <- NA
  expect_error(run_fe(data = bad), "No missings in the 'time'")

  bad <- dat
  bad$unit[3] <- NA
  expect_error(run_fe(data = bad), "No missings in the 'unit'")

  bad <- dat
  bad$tmax[3] <- NA
  expect_error(run_fe(data = bad), "No missings in time")
})

test_that("select_fe validates 're_mod' and 're_id'", {
  expect_error(run_fe(re_mod = 123), "'re_mod' must be either")
  expect_error(run_fe(re_mod = c(RE, RE)), "must be a single string")
  expect_error(
    run_fe(re_mod = fake_select_re(), re_id = c("re1", "re2")),
    "'re_id' must be a single"
  )
  expect_error(
    run_fe(re_mod = fake_select_re(), re_id = "zzz"),
    "'re_id' not found"
  )
})

test_that("select_fe errors when random-effect identifiers are missing", {
  no_season <- dat[, names(dat) != "seasonal_id"]
  expect_error(run_fe(data = no_season), "'seasonal_id' not found")

  no_year <- dat[, names(dat) != "year_id"]
  expect_error(
    run_fe(data = no_year, re_mod = "f(year_id, model = 'iid')"),
    "'year_id' not found"
  )

  no_spatial <- dat[, names(dat) != "spatial_id"]
  expect_error(
    run_fe(data = no_spatial, re_mod = "f(spatial_id, model = 'bym2')"),
    "'spatial_id' not found"
  )
})

test_that("select_fe errors when the spatial graph 'g' is not available", {
  skip_if(exists("g"), "an object named 'g' is visible to select_fe()")
  expect_error(
    run_fe(re_mod = "f(spatial_id, model = 'bym2', graph = g)"),
    "Spatial graph 'g' not found"
  )
})

test_that("select_fe validates roll_mean, roll_sum, lag and nl", {
  expect_error(run_fe(roll_mean = 3), "must be NULL or a list")
  expect_error(run_fe(roll_sum = 3), "must be NULL or a list")
  expect_error(
    run_fe(roll_mean = list(nope = 3)),
    "must be in covariates"
  )
  expect_error(run_fe(roll_mean = list(tmax = 0)), "larger than")
  expect_error(run_fe(roll_mean = list(tmax = 1.5)), "integers")
  expect_error(run_fe(lag = list(tmax = -1)), "larger than")
  expect_error(run_fe(nl = list(tmax = 2)), "larger than")
})

test_that("select_fe validates 'metric' and 'standardize'", {
  expect_error(run_fe(metric = "nope"), "Invalid chosen metric")
  expect_error(run_fe(standardize = 1), "must be character")
  expect_error(run_fe(standardize = "nope"), "must be in 'covariates'")
})


### 1. Output structure and forwarded arguments ----

test_that("select_fe returns a well-formed GHRselect_fe object", {
  res <- run_fe()

  expect_s3_class(res, "GHRselect_fe")
  expect_named(
    res,
    c(
      "params",
      "data",
      "fe_uni_mod",
      "fe_uni_gof",
      "fe_uni_selected",
      "fe_multi_mod",
      "fe_multi_gof",
      "fe_multi_selected",
      "best"
    )
  )
  expect_equal(res$params$unit, "unit")
  expect_equal(res$params$time, "date")
  expect_equal(res$params$metric, "waic")
  expect_equal(unname(res$params$re_form), RE)
  expect_named(res$fe_uni_selected, c("pdsi", "tmax"))
})

test_that("select_fe forwards family, offset_var and nthreads to both fits", {
  cap <- new.env()
  run_fe(cap, family = "poisson", offset_var = "pop", nthreads = 2)

  expect_length(cap$calls, 2)
  for (call in cap$calls) {
    expect_equal(call$family, "poisson")
    expect_equal(call$offset_var, "pop")
    expect_equal(call$nthreads, 2)
    expect_true("pop" %in% names(call$data))
  }
})

test_that("select_fe uses the best or the requested model of a select_re output", {
  re <- fake_select_re()

  res_best <- run_fe(re_mod = re)
  expect_equal(unname(res_best$params$re_form), RE)

  res_id <- run_fe(re_mod = re, re_id = "re2")
  expect_equal(unname(res_id$params$re_form), "f(year_id, model = 'iid')")
})

test_that("select_fe works for a single time series (unit = NULL)", {
  one <- dat[dat$unit == "u1", ]
  res <- run_fe(
    data = one,
    unit = NULL,
    lag = list(tmax = 1),
    roll_mean = list(tmax = 3)
  )
  expect_s3_class(res, "GHRselect_fe")
  expect_true("tmax.rmean3.l1" %in% names(res$data))
})

test_that("select_fe does not duplicate columns when 'unit' is also a random-effect id", {
  res <- run_fe(unit = "year_id", re_mod = "f(year_id, model = 'iid')")
  expect_false(anyDuplicated(names(res$data)) > 0)
  expect_equal(sum(names(res$data) == "year_id"), 1)
})


### 2. Rolling statistics and lags ----

test_that("select_fe only includes 'noroll' when a covariate is in neither roll list", {
  res <- run_fe(roll_mean = list(tmax = c(3, 6)), standardize = NULL)
  nms <- names(res$data)

  expect_true(all(c("tmax.rmean3.l0", "tmax.rmean6.l0") %in% nms))
  expect_false("tmax.noroll.l0" %in% nms)
  # pdsi is in neither list
  expect_true("pdsi.noroll.l0" %in% nms)
  expect_false(any(grepl("^pdsi\\.r", nms)))
})

test_that("select_fe combines roll_mean and roll_sum and keeps a single 'noroll'", {
  res <- run_fe(
    roll_mean = list(tmax = c(1, 3)),
    roll_sum = list(tmax = c(1, 3)),
    standardize = NULL
  )
  tmax_cols <- grep("^tmax\\.", names(res$data), value = TRUE)

  expect_setequal(
    tmax_cols,
    c("tmax.noroll.l0", "tmax.rmean3.l0", "tmax.rsum3.l0")
  )
})

test_that("select_fe rolls within each unit and does not mix units", {
  res <- run_fe(
    roll_mean = list(tmax = 3),
    roll_sum = list(pdsi = 3),
    standardize = NULL
  )

  for (u in c("u1", "u2")) {
    raw <- dat[dat$unit == u, ]
    out <- res$data[res$data$unit == u, ]
    # First retained row is the first complete window of the unit
    expect_equal(out$tmax.rmean3.l0[1], mean(raw$tmax[1:3]))
    expect_equal(out$pdsi.rsum3.l0[1], sum(raw$pdsi[1:3]))
    expect_equal(nrow(out), n_days - 2)
  }
})

test_that("select_fe lags within each unit", {
  res <- run_fe(lag = list(tmax = c(1, 2)), standardize = NULL)

  expect_true(all(c("tmax.noroll.l1", "tmax.noroll.l2") %in% names(res$data)))
  # A covariate not in 'lag' only gets the non-lagged version
  expect_true("pdsi.noroll.l0" %in% names(res$data))

  for (u in c("u1", "u2")) {
    raw <- dat[dat$unit == u, ]
    out <- res$data[res$data$unit == u, ]
    # The first 2 time points of each unit are removed (largest lag)
    expect_equal(out$tmax.noroll.l1, raw$tmax[2:(n_days - 1)])
    expect_equal(out$tmax.noroll.l2, raw$tmax[1:(n_days - 2)])
  }
})

test_that("select_fe removes and reports the time points lost per unit", {
  msgs <- testthat::capture_messages(
    res <- run_fe(
      roll_mean = list(tmax = 3),
      lag = list(tmax = 2),
      .quiet = FALSE
    )
  )
  # Roll (3 - 1) plus lag 2 apply to the same covariate
  expect_match(paste(msgs, collapse = ""), "first 4 time points")
  expect_equal(nrow(res$data), n_units * (n_days - 4))
})

test_that("select_fe only adds rolls and lags that belong to the same covariate", {
  msgs <- testthat::capture_messages(
    res <- run_fe(
      roll_mean = list(tmax = 6),
      lag = list(pdsi = 3),
      .quiet = FALSE
    )
  )
  # max(6 - 1, 3) and not (6 - 1) + 3
  expect_match(paste(msgs, collapse = ""), "first 5 time points")
  expect_equal(nrow(res$data), n_units * (n_days - 5))
})

test_that("select_fe does not drop rows or report losses without rolls or lags", {
  msgs <- testthat::capture_messages(res <- run_fe(.quiet = FALSE))
  expect_false(any(grepl("time points", msgs)))
  expect_equal(nrow(res$data), nrow(dat))
})

test_that("select_fe keeps rows with a missing outcome when rolls and lags are used", {
  na_dat <- dat
  na_dat$y[6] <- NA
  res <- run_fe(
    data = na_dat,
    roll_mean = list(tmax = 3),
    lag = list(tmax = 1)
  )
  expect_true(anyNA(res$data$y))
  expect_equal(nrow(res$data), n_units * (n_days - 3))
})


### 3. Standardization ----

test_that("select_fe standardizes all covariates by default", {
  msgs <- testthat::capture_messages(res <- run_fe(.quiet = FALSE))
  expect_match(
    paste(msgs, collapse = ""),
    "Standardized covariates: tmax, pdsi"
  )

  for (v in c("tmax.noroll.l0", "pdsi.noroll.l0")) {
    expect_equal(mean(res$data[[v]]), 0)
    expect_equal(stats::sd(res$data[[v]]), 1)
  }
})

test_that("select_fe standardizes only the requested covariates", {
  res_one <- run_fe(standardize = "tmax")
  expect_equal(stats::sd(res_one$data$tmax.noroll.l0), 1)
  expect_equal(res_one$data$pdsi.noroll.l0, dat$pdsi)

  # More than one covariate name is allowed
  res_two <- run_fe(standardize = c("tmax", "pdsi"))
  expect_equal(stats::sd(res_two$data$tmax.noroll.l0), 1)
  expect_equal(stats::sd(res_two$data$pdsi.noroll.l0), 1)
})

test_that("select_fe does not standardize when standardize = NULL", {
  msgs <- testthat::capture_messages(
    res <- run_fe(standardize = NULL, .quiet = FALSE)
  )
  expect_match(
    paste(msgs, collapse = ""),
    "No covariates have been standardized"
  )
  expect_equal(res$data$tmax.noroll.l0, dat$tmax)
  expect_equal(res$data$pdsi.noroll.l0, dat$pdsi)
})


### 4. Formula generation, ranking and selection ----

test_that("select_fe applies non-linear terms only to the requested covariate", {
  cap <- new.env()
  d <- dat
  d$temp <- d$tmax
  d$temp_max <- d$pdsi
  run_fe(
    cap,
    data = d,
    covariates = c("temp", "temp_max"),
    nl = list(temp = 4)
  )

  uni_terms <- unlist(cap$calls[[1]]$formulas$vars, use.names = FALSE)
  uni_terms <- uni_terms[!is.na(uni_terms)]
  # as_GHRformulas() labels non-linear terms as '<var>_nl_q<n>_<model>'
  expect_true(any(grepl("^temp\\.noroll\\.l0_nl_q4", uni_terms)))
  # 'temp_max' shares the prefix 'temp' but must stay linear
  expect_false(any(grepl("^temp_max.*_nl_q", uni_terms)))
})

test_that("select_fe selects the best univariable term per covariate", {
  res <- run_fe(roll_mean = list(tmax = c(1, 3)))

  # The fake fit_models() prefers terms containing 'rmean3'
  expect_equal(unname(res$fe_uni_selected$tmax), "tmax.rmean3.l0")
  expect_equal(unname(res$fe_uni_selected$pdsi), "pdsi.noroll.l0")
  # The baseline never appears in the univariable results
  expect_false("baseline" %in% res$fe_uni_gof$covariate)
})

test_that("select_fe never ranks or selects the baseline as a multivariable model (regression test)", {
  res <- run_fe(roll_mean = list(tmax = c(1, 3)))

  # The baseline is mod1 and is the best model on every metric in the fake
  expect_false("mod1" %in% res$fe_multi_gof$model_id)
  expect_false(anyNA(res$fe_multi_gof$effects))
  expect_false(anyNA(res$fe_multi_selected))
  expect_false(anyNA(res$best))
  expect_false(names(res$best) == "mod1")
  expect_equal(
    sort(res$fe_multi_gof$final_rank),
    seq_len(nrow(res$fe_multi_gof))
  )
  expect_equal(unname(res$best), "tmax.rmean3.l0")
})

test_that("select_fe returns n_select_multi ordered candidates and the best one", {
  res <- run_fe(roll_mean = list(tmax = c(1, 3)), n_select_multi = 2)

  expect_length(res$fe_multi_selected, 2)
  expect_equal(names(res$fe_multi_selected)[1], names(res$best))
  expect_equal(res$fe_multi_selected[[1]], res$best[[1]])

  # Asking for more candidates than models returns all of them
  res_all <- run_fe(roll_mean = list(tmax = c(1, 3)), n_select_multi = 100)
  expect_length(res_all$fe_multi_selected, nrow(res_all$fe_multi_gof))
})

test_that("select_fe keeps n_select_uni terms per covariate", {
  res <- run_fe(roll_mean = list(tmax = c(1, 3, 6)), n_select_uni = 2)
  expect_length(res$fe_uni_selected$tmax, 2)
  expect_length(res$fe_uni_selected$pdsi, 1)
})


### 5. Internal helper .tidy_model_terms ----

test_that(".tidy_model_terms attributes terms to the right covariate", {
  terms <- data.frame(
    covariate_1 = c(
      "tmax.noroll.l0",
      "f(INLA::inla.group(tmax.noroll.l0, method='quantile', n=4), model='rw2')",
      "max.rmean3.l1",
      "temp_max.noroll.l0",
      "temp.noroll.l0"
    )
  )
  out <- .tidy_model_terms(
    terms,
    pattern = c("max", "tmax", "temp", "temp_max"),
    covariate_cols = "covariate_1",
    rank_by_pattern = FALSE
  )
  expect_equal(
    unname(out$covariate),
    c("tmax", "tmax", "max", "temp_max", "temp")
  )
})

test_that(".tidy_model_terms matches covariate names literally", {
  out <- .tidy_model_terms(
    data.frame(covariate_1 = c("aXb.noroll.l0", "a.b.noroll.l0")),
    pattern = "a.b",
    covariate_cols = "covariate_1",
    rank_by_pattern = FALSE
  )
  expect_equal(unname(out$covariate), c(NA, "a.b"))
})
