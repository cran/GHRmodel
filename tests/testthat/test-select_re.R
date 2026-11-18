# Tests for select_re().
#
# Fitting through INLA is by far the most expensive part of this pipeline
# (select_re() fits one model per univariable random effect, then one model
# per multivariable combination), so this file is deliberately structured to
# avoid it wherever the logic under test doesn't require a real fit:
#   - Section 0 covers select_re()'s upfront input validation, which always
#     runs (and fails fast) before any model is fit.
#   - Section 1 mocks out '.model_batch()' (the function that actually calls
#     fit_models()/INLA) with a fast fake, so the *real* select_re() ranking,
#     selection, and combination-generation logic runs end-to-end in
#     milliseconds. This directly regression-tests the fixes for: 'nthreads'
#     silently not being forwarded, the outcome-column-order NA-check bug,
#     the 'baseline' group leaking into the multivariable combinations, and
#     the duplicated effect/effects columns in the multivariable output.
#   - Section 2 has a single, deliberately minimal real-INLA smoke test
#     (skipped if INLA isn't installed) confirming the mocked logic above
#     matches what actually happens when fit_models() runs for real.

# ---- Shared fixtures ----

# 3 units x 10 consecutive days each: small enough to be fast, but enough
# structure to exercise spatial/seasonal/interannual identifiers.
n_units <- 3
n_days <- 10
dat <- data.frame(
  micro = rep(paste0("unit", seq_len(n_units)), each = n_days),
  date = rep(as.Date("2020-01-01") + 0:(n_days - 1), times = n_units),
  spatial_id = rep(seq_len(n_units), each = n_days),
  seasonal_id = rep(seq_len(n_days), times = n_units),
  year_id = 1L,
  pop = 1000,
  y = stats::rpois(n_units * n_days, 5)
)

# select_re() looks up 'prior' entries and 'g' with exists(), which only
# finds objects reachable from select_re()'s own lexical ancestors -- when
# tests run inside the package (via testthat::test_file()/test_check()),
# that chain does not reliably include a test file's ordinary top-level
# bindings, only the true global environment. So these fixtures are assigned
# directly into globalenv() rather than just written as 'prec1 <- ...'.
# They are removed again in teardown-select_re.R, once all tests have run.
# 'g' is intentionally *not* defined yet: the first test below relies on it
# not existing.
assign(
  "prec1",
  list(prec = list(prior = "pc.prec", param = c(0.5, 0.01))),
  envir = globalenv()
)


### 0. Input validation (no model fitting involved) ----

test_that("select_re errors when required arguments are missing", {
  expect_error(
    select_re(outcome = "y", prior = "prec1", time = "date", unit = "micro"),
    "`data` is absent"
  )
  expect_error(
    select_re(data = dat, prior = "prec1", time = "date", unit = "micro"),
    "`outcome` is absent"
  )
  expect_error(
    select_re(data = dat, outcome = "y", time = "date", unit = "micro"),
    "`prior` is absent"
  )
  expect_error(
    select_re(data = dat, outcome = "y", prior = "prec1", unit = "micro"),
    "`time` is absent"
  )
})

test_that("select_re errors when 'outcome' is not a column of data", {
  expect_error(
    select_re(
      data = dat,
      outcome = "not_a_col",
      prior = "prec1",
      time = "date",
      unit = "micro"
    ),
    "'outcome' not found in the data"
  )
})

test_that("select_re errors when 'offset_var' is not a column of data", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      offset_var = "not_a_col"
    ),
    "'offset_var' not found in the data"
  )
})

test_that("select_re errors on a missing or NA-containing 'time' column", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "not_a_col",
      unit = "micro"
    ),
    "No column of the data matches the 'time' argument"
  )

  bad <- dat
  bad$date[1] <- NA
  expect_error(
    select_re(
      data = bad,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro"
    ),
    "No missings in the 'time' columns are permitted."
  )
})

test_that("select_re errors on a missing or NA-containing 'unit' column", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "not_a_col"
    ),
    "No column of the data matches the 'unit' argument"
  )

  bad <- dat
  bad$micro[1] <- NA
  expect_error(
    select_re(
      data = bad,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro"
    ),
    "No missings in the 'unit' columns are permitted."
  )
})

test_that("select_re forces re_spatial to NULL (with a message) when 'unit' is NULL", {
  # unit = NULL forces re_spatial <- NULL regardless of what was requested;
  # with re_seasonal/re_interannual also NULL this falls through to the
  # "at least one RE" error before any model is fit, so we observe the
  # message without paying for a single INLA call. unit = NULL means
  # check_consecutive() treats the whole data set as *one* series, so a
  # single unit's worth of (unique, consecutive) dates is used here.
  one_series <- dat[dat$micro == "unit1", ]
  expect_message(
    expect_error(
      select_re(
        data = one_series,
        outcome = "y",
        prior = "prec1",
        time = "date",
        unit = NULL,
        re_spatial = "bym2",
        re_seasonal = NULL,
        re_interannual = NULL
      ),
      "At least one of the random effects must be non-null"
    ),
    "Setting 're_spatial' to NULL"
  )
})

test_that("select_re errors when 'spatial_id' is missing from the data", {
  expect_error(
    select_re(
      data = dplyr::select(dat, -spatial_id),
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_seasonal = NULL,
      re_interannual = NULL
    ),
    "'spatial_id' not found in the data"
  )
})

test_that("select_re errors when spatial graph 'g' is not available", {
  # 'g' has deliberately not been created yet at this point in the file.
  expect_false(exists("g"))
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_seasonal = NULL,
      re_interannual = NULL
    ),
    "Spatial graph 'g' not found in the environment"
  )
})

# 'g' is needed for every remaining test that leaves re_spatial != NULL.
# Also removed in teardown-select_re.R.
assign("g", matrix(0, n_units, n_units), envir = globalenv())

test_that("select_re errors on an invalid 're_spatial' model", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = "not_a_model",
      re_seasonal = NULL,
      re_interannual = NULL
    ),
    "Invalid spatial model"
  )
})

test_that("select_re errors on invalid seasonal random-effect inputs", {
  expect_error(
    select_re(
      data = dplyr::select(dat, -seasonal_id),
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "rw1",
      re_interannual = NULL
    ),
    "'seasonal_id' not found in the data"
  )
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "not_a_model",
      re_interannual = NULL
    ),
    "Invalid seasonal model"
  )
})

test_that("select_re errors on invalid interannual random-effect inputs", {
  expect_error(
    select_re(
      data = dplyr::select(dat, -year_id),
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = NULL,
      re_interannual = "iid"
    ),
    "'year_id' not found in the data"
  )
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = NULL,
      re_interannual = "not_a_model"
    ),
    "Invalid interannual model"
  )
})

test_that("select_re errors when all random effects are NULL", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = NULL,
      re_interannual = NULL
    ),
    "At least one of the random effects must be non-null."
  )
})

test_that("select_re errors when a named prior is not available in the environment", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "not_a_prior_object",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "rw1",
      re_interannual = NULL
    ),
    "Prior not_a_prior_object not available in the environment."
  )
})

test_that("select_re errors on an invalid 'metric'", {
  expect_error(
    select_re(
      data = dat,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "rw1",
      re_interannual = NULL,
      metric = "not_a_metric"
    ),
    "Invalid chosen metric."
  )
})


### 1. Data-preprocessing regression test (still no model fitting) ----

test_that("select_re detects NAs in non-outcome columns regardless of column order in 'data'", {
  # Regression test: the missingness check used to assume 'outcome' was
  # physically the first column of the data subset, which only held when
  # 'outcome' happened to be the first matching column of the *original*
  # data.frame. Here 'micro' (not 'y') is first, so under the old logic this
  # NA in 'micro' would have been (wrongly) excluded from the check instead
  # of 'y'.
  bad <- dat[, c(
    "micro",
    "spatial_id",
    "y",
    "seasonal_id",
    "year_id",
    "date",
    "pop"
  )]
  bad$micro[1] <- NA
  expect_error(
    select_re(
      data = bad,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "rw1",
      re_interannual = NULL
    ),
    "No missings"
  )
})


### 2. Mocked-fitting tests (real select_re() logic, fake .model_batch()) ----

# A minimal stand-in for '.model_batch()' that skips INLA entirely: for each
# named 'effect' it fabricates a one-row GoF data.frame with random 'waic'/
# 'lms' values, keeping every other bit of select_re()'s own logic (formula
# generation, ranking, selection, combination, joining, output assembly)
# real. It also records the 'nthreads' it was called with, so tests can
# check that select_re() actually forwards it.
make_fake_model_batch <- function(capture_env) {
  function(
    outcome,
    effect,
    data,
    family,
    offset_var,
    control_compute,
    nthreads
  ) {
    capture_env$values <- c(capture_env$values, nthreads)
    mod_list <- list()
    gof_list <- list()
    for (i in seq_along(effect)) {
      id <- names(effect)[i]
      mod_list[[id]] <- structure(
        list(mod_gof = data.frame(model_id = id)),
        class = "GHRmodels"
      )
      gof_list[[id]] <- data.frame(
        model_id = id,
        waic = stats::runif(1, 100, 200),
        lms = stats::runif(1, 1, 2)
      )
    }
    list(mod_list = mod_list, gof_list = gof_list)
  }
}

run_mocked_select_re <- function(capture_env, ...) {
  testthat::local_mocked_bindings(
    .model_batch = make_fake_model_batch(capture_env),
    stack_models = function(x) x,
    .package = "GHRmodel"
  )
  select_re(
    data = dat,
    outcome = "y",
    prior = "prec1",
    time = "date",
    unit = "micro",
    re_spatial = "bym2",
    re_seasonal = c("rw1", "rw2"),
    re_interannual = c("iid", "rw1"),
    ...
  )
}

test_that("select_re returns a well-formed GHRselect_re object", {
  capture_env <- new.env()
  res <- run_mocked_select_re(capture_env)

  expect_s3_class(res, "GHRselect_re")
  expect_named(
    res,
    c(
      "params",
      "data",
      "re_uni_mod",
      "re_uni_gof",
      "re_uni_selected",
      "re_multi_mod",
      "re_multi_gof",
      "re_multi_selected",
      "best"
    )
  )
  expect_true(all(
    c("spatial", "seasonal", "interannual") %in% names(res$re_uni_selected)
  ))
})

test_that("select_re forwards 'nthreads' to every model-fitting stage (regression test)", {
  capture_env <- new.env()
  # NB: passed as 'nthreads = 3' via '...', not as a second positional
  # argument -- see the 'capture_env' (not 'nthreads_log') parameter name
  # above, which avoids R partially matching "nthreads" against a formal
  # that starts with it instead of routing it through '...'.
  run_mocked_select_re(capture_env, nthreads = 3)

  # One call to '.model_batch()' for the univariable stage, one for the
  # multivariable stage; both must see the user-supplied value, not the
  # internal default of 8.
  expect_length(capture_env$values, 2)
  expect_true(all(capture_env$values == 3))
})

test_that("select_re excludes the baseline model from the selectable RE groups (regression test)", {
  capture_env <- new.env()
  res <- run_mocked_select_re(capture_env)

  expect_false("baseline" %in% names(res$re_uni_selected))
  expect_false(any(grepl("^baseline", names(res$re_multi_gof))))
})

test_that("select_re's multivariable GoF has a single, non-duplicated formula column (regression test)", {
  capture_env <- new.env()
  res <- run_mocked_select_re(capture_env)

  expect_false("effect" %in% names(res$re_multi_gof))
  expect_true("effects" %in% names(res$re_multi_gof))
})

test_that("select_re's 'best' is the top entry of 're_multi_selected' (regression test)", {
  capture_env <- new.env()
  res <- run_mocked_select_re(capture_env, n_select_multi = 3)

  expect_identical(res$best, res$re_multi_selected[1])
})

test_that("select_re allows NAs in 'outcome' regardless of column order (regression test)", {
  capture_env <- new.env()
  bad <- dat[, c(
    "micro",
    "spatial_id",
    "y",
    "seasonal_id",
    "year_id",
    "date",
    "pop"
  )]
  bad$y[1] <- NA

  testthat::local_mocked_bindings(
    .model_batch = make_fake_model_batch(capture_env),
    stack_models = function(x) x,
    .package = "GHRmodel"
  )
  expect_no_error(
    select_re(
      data = bad,
      outcome = "y",
      prior = "prec1",
      time = "date",
      unit = "micro",
      re_spatial = NULL,
      re_seasonal = "rw1",
      re_interannual = NULL
    )
  )
})


test_that(".stack_models_safe returns a lone model as-is instead of erroring (stack_models guard)", {
  # stack_models() itself requires >= 2 'GHRmodels' objects (by design, per
  # its own docs) and errors otherwise. select_re()'s multivariable stage
  # can legitimately end up with just one fitted model (e.g. a single
  # non-baseline random-effect group), so '.stack_models_safe()' is what
  # select_re() actually calls, and it must tolerate that case.
  one_model <- structure(
    list(mod_gof = data.frame(model_id = "mod01")),
    class = "GHRmodels"
  )

  expect_identical(.stack_models_safe(list(one_model)), one_model)
  expect_error(stack_models(list(one_model)), "At least two")
})

test_that(".stack_models_safe delegates to stack_models() when there are 2+ models", {
  make_model <- function(id) {
    structure(
      list(
        mod_gof = data.frame(model_id = id, waic = 1),
        fitted = stats::setNames(list(1), id),
        fixed = stats::setNames(list(1), id),
        random = stats::setNames(list(1), id),
        vcov = stats::setNames(list(NULL), id),
        formulas = stats::setNames("y ~ 1", id),
        inlamod = stats::setNames(list(NULL), id),
        re = id,
        ppd = stats::setNames(list(NULL), id),
        family = "gaussian",
        outcome = "y",
        data = data.frame(y = 1:3)
      ),
      class = "GHRmodels"
    )
  }
  m1 <- make_model("mod01")
  m2 <- make_model("mod02")

  expect_identical(
    .stack_models_safe(list(m1, m2)),
    stack_models(list(m1, m2))
  )
})


### 3. Minimal real-INLA smoke test ----

test_that("select_re runs end-to-end with a real (minimal) INLA fit", {
  skip_if_not_installed("INLA")

  # Deliberately the smallest possible RE search space (a single group, a
  # single prior): 2 univariable models (baseline + 1) and, thanks to the
  # '.stack_models_safe()' guard, exactly 1 multivariable "combination"
  # (the group on its own) rather than erroring out on stack_models()'s
  # 'at least two models' requirement.
  res <- select_re(
    data = dat,
    outcome = "y",
    prior = "prec1",
    time = "date",
    unit = "micro",
    re_spatial = NULL,
    re_seasonal = "rw1",
    re_interannual = NULL,
    offset_var = "pop"
  )

  expect_s3_class(res, "GHRselect_re")
  expect_s3_class(res$re_uni_mod, "GHRmodels")
  expect_s3_class(res$re_multi_mod, "GHRmodels")
  expect_true(is.character(res$best) && length(res$best) == 1)
  expect_false("baseline" %in% names(res$re_uni_selected))
})
