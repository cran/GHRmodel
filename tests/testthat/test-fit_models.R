# Load data
data("dengue_MS", package = "GHRmodel")
data("map_MS", package = "GHRmodel")

# Example: Filter data
dengue_MS <- dplyr::filter(dengue_MS, year > 2017)

# Basic transformations
dengue_MS$spat_id <- as.numeric(as.factor((dengue_MS$micro_code)))
dengue_MS$spat_meso_id <- as.numeric(as.factor(dengue_MS$meso_code))
dengue_MS$year_id <- as.numeric(as.factor(dengue_MS$year))
dengue_MS$year_id2 <- as.numeric(as.factor(dengue_MS$year))
dengue_MS$month_id <- as.numeric(as.factor(dengue_MS$month))
dengue_MS$main_climate_f <- as.numeric(as.factor(dengue_MS$main_climate))

# Define multiple predictors. This file tests fit_models()'s fitting-loop
# mechanics (GoF columns, list lengths, the config path), not covariate
# parsing (already covered without INLA in test-as_GHRformulas.R and
# test-write_inla_formulas.R), so a handful of representative formula
# shapes -- plain linear, non-linear inla.group with a replicate,
# multi-covariate with an interaction, and a spatially varying coefficient
# -- is enough to exercise each fitting code path once.
list_cov <- list(
  c("tmin"),
  c(
    "f(INLA::inla.group(pdsi, method='quantile', n=10), model='rw2', replicate =main_climate_f)"
  ),
  c("tmin", "pdsi", "pdsi:tmin"),
  c("f(main_climate_f,tmin, model = 'iid')")
)

#Tranform them into inla formulas
formulas <- write_inla_formulas(
  outcome = "dengue_cases",
  covariates = list_cov,
  re1 = list(id = "month_id", scale = TRUE, model = "rw1", cyclic = TRUE),
  re2 = list(id = "year_id", model = "iid"),
  baseline = TRUE
)

# Convert to GHRformulas
ghr_forms <- as_GHRformulas(formulas)

test_that("fit_models works with config = FALSE", {
  # Skip if INLA/spdep not installed
  skip_if_not_installed("INLA")

  # Fit multiple models with fit_models
  results <- fit_models(
    formulas = ghr_forms,
    data = dengue_MS,
    family = "nbinomial",
    name = "TestModel",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE),
    progress = TRUE
  )

  # Basic checks on returned object
  expect_s3_class(results, "GHRmodels")
  expect_true(is.list(results), "Returned object should be a list")

  # Check mod_gof structure
  expect_true("mod_gof" %in% names(results), "Should contain 'mod_gof'")
  # Typical columns: dic, waic, lms, rsq
  gof_cols <- c("dic", "waic", "lms", "r2dev", "mae", "rmse", "crps")
  for (col in gof_cols) {
    expect_true(
      col %in% colnames(results$mod_gof),
      paste("Column missing in 'mod_gof':", col)
    )
  }

  # Ensure we have as many model results as formulas
  n_forms <- length(formulas)

  expect_length(results$fitted, n_forms)
  expect_length(results$fixed, n_forms)
  expect_length(results$random, n_forms)
  expect_length(results$formulas, n_forms)
})


test_that("fit_models works with config = TRUE", {
  # Skip if INLA/spdep not installed
  skip_if_not_installed("INLA")

  # Fit multiple models with fit_models
  results <- fit_models(
    formulas = ghr_forms,
    data = dengue_MS,
    family = "nbinomial",
    name = "TestModel",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = TRUE),
    progress = TRUE
  )

  # Basic checks on returned object
  expect_s3_class(results, "GHRmodels")
  expect_true(is.list(results), "Returned object should be a list")

  # Check mod_gof structure
  expect_true("mod_gof" %in% names(results), "Should contain 'mod_gof'")
  # Typical columns: dic, waic, lms, rsq, mae, rmse, crps
  gof_cols <- c("dic", "waic", "lms", "r2dev", "mae", "rmse", "crps")
  for (col in gof_cols) {
    expect_true(
      col %in% colnames(results$mod_gof),
      paste("Column missing in 'mod_gof':", col)
    )
  }

  # Ensure we have as many model results as formulas
  n_forms <- length(formulas)

  expect_length(results$fitted, n_forms)
  expect_length(results$fixed, n_forms)
  expect_length(results$random, n_forms)
  expect_length(results$formulas, n_forms)
})


test_that("fit_models 'verbose' argument is applied to every fitted model", {
  # Skip if INLA/spdep not installed
  skip_if_not_installed("INLA")

  # Use config=TRUE to get back the raw 'inla' objects for both
  # models fitted from two formulas: model 1 is fit directly, model 2 goes
  # through the models 2:n loop. Each 'inla' object records the 'verbose'
  # value it was actually called with in '$.args$verbose'.
  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    offset_var = "pop100000",
    nthreads = 2,
    keep_inlamod = TRUE,
    verbose = TRUE
  )

  expect_length(results$inlamod, 2)
  # Regression test: 'verbose' was previously dropped when forwarded to
  # '.fit_single_model()' inside the models 2:n loop, so only the first
  # model ever ran in verbose mode.
  for (mod in results$inlamod) {
    expect_true(mod$.args$verbose)
  }
})

test_that("fit_models logs 'offset_var' for poisson but not for gaussian", {
  skip_if_not_installed("INLA")

  # The offset is computed once via '.process_offset()' and reused for both
  # model 1 and the models 2:n loop, so checking both models' 'inla' objects
  # (via keep_inlamod) also confirms the pre-computed value is
  # forwarded unchanged to every model, not just the first.
  results_poisson <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "poisson",
    offset_var = "pop100000",
    nthreads = 2,
    keep_inlamod = TRUE
  )
  for (mod in results_poisson$inlamod) {
    expect_equal(mod$.args$offset, log(dengue_MS$pop100000))
  }

  results_gaussian <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "gaussian",
    offset_var = "pop100000",
    nthreads = 2,
    keep_inlamod = TRUE
  )
  for (mod in results_gaussian$inlamod) {
    expect_equal(mod$.args$offset, dengue_MS$pop100000)
  }
})


test_that("fit_models works with ppd_samples, keep_ppd, keep_inlamod", {
  skip_if_not_installed("INLA")

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    name = "TestModel",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = TRUE),
    ppd_samples = 50,
    keep_ppd = TRUE,
    keep_inlamod = TRUE,
    progress = TRUE
  )

  # 1. Exists
  expect_true("ppd" %in% names(results))
  expect_true("inlamod" %in% names(results))

  # 2. Is list
  expect_type(results$ppd, "list")
  expect_type(results$inlamod, "list")

  # 3. Correct length
  expect_length(results$ppd, 2L)
  expect_length(results$inlamod, 2L)

  # 4. Not all NULL
  all_null <- all(vapply(results$ppd, is.null, logical(1)))
  expect_false(all_null)
  all_null <- all(vapply(results$inlamod, is.null, logical(1)))
  expect_false(all_null)

  # 5. Not all NA (extra safety, though uncommon for lists)
  all_na <- all(vapply(
    results$inlamod,
    function(x) {
      length(x) == 1 && is.na(x)
    },
    logical(1)
  ))

  # 6. Correct dims ppd
  expect_identical(dim(results$ppd[[1]]), c(nrow(dengue_MS), 50L))

  # 8. Index error
  expect_error(
    {
      fit_models(
        formulas = as_GHRformulas(formulas[1:2]),
        data = dengue_MS,
        family = "nbinomial",
        name = "TestModel",
        offset_var = "pop100000",
        nthreads = 2,
        control_compute = list(config = TRUE),
        ppd_samples = 100,
        keep_ppd = c("TRUE", "TRUE"),
        keep_inlamod = TRUE,
        progress = TRUE
      )
    },
    "single logical value"
  )

  # 9. A logical vector (as opposed to a single logical) must also error
  expect_error(
    fit_models(
      formulas = as_GHRformulas(formulas[1:2]),
      data = dengue_MS,
      family = "nbinomial",
      name = "TestModel",
      offset_var = "pop100000",
      nthreads = 2,
      control_compute = list(config = TRUE),
      keep_ppd = c(TRUE, FALSE),
      progress = TRUE
    ),
    "single logical value"
  )
})


### Input validation (no model fitting involved) ----

test_that("fit_models errors when required arguments are missing or invalid", {
  # formulas/data/family have no default, so omitting them raises R's own
  # missing-argument error rather than a package-specific message.
  expect_error(
    fit_models(data = dengue_MS, family = "nbinomial"),
    "formulas"
  )
  expect_error(
    fit_models(formulas = ghr_forms, family = "nbinomial"),
    "data"
  )
  expect_error(
    fit_models(formulas = ghr_forms, data = "not_a_df", family = "nbinomial"),
    "'data' must be a data.frame."
  )
  expect_error(
    fit_models(formulas = ghr_forms, data = dengue_MS),
    "family"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = c("poisson", "nbinomial")
    ),
    "'family' must be a single character string."
  )
  expect_error(
    fit_models(formulas = ghr_forms, data = dengue_MS, family = "not_a_family"),
    "Invalid 'family'."
  )
})

test_that("fit_models errors on invalid 'name'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      name = 1
    ),
    "'name' must be a character."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      name = c("a", "b")
    ),
    "'name' must have either length 1 or the number"
  )
  expect_error(
    fit_models(
      formulas = as_GHRformulas(formulas[1:2]),
      data = dengue_MS,
      family = "nbinomial",
      name = c("a", "a")
    ),
    "No duplicates in 'name' are allowed."
  )
})

test_that("fit_models errors on invalid 'offset_var'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      offset_var = "not_a_column"
    ),
    "must be a character string that exists in 'data'"
  )
})

test_that("fit_models errors when the deprecated 'offset' argument is used", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      offset = "pop100000"
    ),
    "'offset' has been deprecated, please use 'offset_var'"
  )
})

test_that("fit_models errors when the offset variable contains NAs", {
  dat <- dengue_MS
  dat$pop_na <- dat$pop100000
  dat$pop_na[1] <- NA
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dat,
      family = "nbinomial",
      offset_var = "pop_na"
    ),
    "The offset variable cannot contain missing values"
  )
})

test_that("fit_models errors when the offset variable is not numeric", {
  dat <- dengue_MS
  dat$pop_chr <- as.character(dat$pop100000)
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dat,
      family = "nbinomial",
      offset_var = "pop_chr"
    ),
    "The offset variable must be numeric"
  )
})

test_that("fit_models errors on offset values <= 0 unless family is gaussian", {
  for (bad in c(0, -1)) {
    dat <- dengue_MS
    dat$pop_bad <- dat$pop100000
    dat$pop_bad[1] <- bad
    expect_error(
      fit_models(
        formulas = ghr_forms,
        data = dat,
        family = "nbinomial",
        offset_var = "pop_bad"
      ),
      "Offset values <=0 have been detected"
    )
  }
  # Gaussian uses the identity link, so the offset is left untouched
  expect_equal(
    .process_offset("pop_bad", dat, "gaussian"),
    dat$pop_bad
  )
})

test_that("fit_models errors when the outcome is not numeric", {
  dat <- dengue_MS
  dat$dengue_cases <- as.character(dat$dengue_cases)
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dat,
      family = "nbinomial"
    ),
    "The outcome variable must be numeric"
  )
})

test_that("fit_models errors on non-count outcomes unless family is gaussian", {
  for (bad in c(2.5, -2)) {
    dat <- dengue_MS
    dat$dengue_cases[1] <- bad
    for (fam in c("nbinomial", "poisson")) {
      expect_error(
        fit_models(formulas = ghr_forms, data = dat, family = fam),
        "the outcome must consist of counts equal to or larger than 0"
      )
    }
  }
  # Gaussian accepts non-integer and negative outcomes: the outcome check
  # must not be the reason for failure
  dat <- dengue_MS
  dat$dengue_cases[1] <- -2.5
  expect_no_error(
    GHRmodel:::.check_inputs_models(
      data = dat,
      family = "gaussian",
      name = "mod",
      n_formulas = 1,
      offset_var = NULL,
      nthreads = 1,
      progress = FALSE,
      verbose = FALSE,
      outcome_var = "dengue_cases",
      formulas = "dengue_cases ~ tmin",
      offset = NULL,
      control_inla = list(),
      control_family = list()
    )
  )
})

test_that("fit_models accepts NA in the count outcome", {
  dat <- dengue_MS
  dat$dengue_cases[1] <- NA
  expect_no_error(
    GHRmodel:::.check_inputs_models(
      data = dat,
      family = "nbinomial",
      name = "mod",
      n_formulas = 1,
      offset_var = NULL,
      nthreads = 1,
      progress = FALSE,
      verbose = FALSE,
      outcome_var = "dengue_cases",
      formulas = "dengue_cases ~ tmin",
      offset = NULL,
      control_inla = list(),
      control_family = list()
    )
  )
})

test_that("fit_models errors when the outcome is not in 'data'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS[, setdiff(names(dengue_MS), "dengue_cases")],
      family = "nbinomial"
    ),
    "The outcome variable dengue_cases has not been found in the data"
  )
})

test_that("fit_models errors on invalid 'control_compute'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_compute = "not_a_list"
    ),
    "'control_compute' must be a named list."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_compute = list(config = FALSE, foo = TRUE)
    ),
    "'control_compute' list must only include names"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_compute = list(config = "yes")
    ),
    "'config' in 'control_compute' must be a logical value."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_compute = list(vcov = "yes")
    ),
    "'vcov' in 'control_compute' must be a logical value."
  )
})

test_that("fit_models errors on invalid 'nthreads', 'progress', 'verbose' and 'ppd_samples'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      nthreads = -1
    ),
    "'nthreads' must be a single positive integer."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      nthreads = NA
    ),
    "'nthreads' must be a single positive integer."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      progress = "yes"
    ),
    "'progress' must be a single logical value"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      verbose = "yes"
    ),
    "'verbose' must be a single logical value"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      verbose = c(TRUE, FALSE)
    ),
    "'verbose' must be a single logical value"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      ppd_samples = -5
    ),
    "'ppd_samples' must be a single positive integer."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      ppd_samples = NA
    ),
    "'ppd_samples' must be a single positive integer."
  )
})

test_that("fit_models errors when keep_ppd is used without config=TRUE", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      keep_ppd = TRUE
    ),
    "'keep_ppd' can only be used when 'config=TRUE'."
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      keep_inlamod = "yes"
    ),
    "'keep_inlamod' must be a single logical value."
  )
})


### Behavioral tests (require INLA) ----

test_that("fit_models aborts entirely if the first (baseline) formula fails to fit", {
  skip_if_not_installed("INLA")

  broken_formulas <- as_GHRformulas(c(
    "dengue_cases ~ log(tmin, base = 'a')",
    "dengue_cases ~ tmin"
  ))

  expect_error(
    fit_models(
      formulas = broken_formulas,
      data = dengue_MS,
      family = "nbinomial",
      name = "Broken",
      offset_var = "pop100000",
      nthreads = 2,
      control_compute = list(config = FALSE),
      progress = FALSE
    )
  )
})

test_that("fit_models skips a later formula that fails to fit and keeps the rest", {
  skip_if_not_installed("INLA")

  mixed_formulas <- as_GHRformulas(c(
    "dengue_cases ~ tmin",
    "dengue_cases ~ log(tmin, base = 'a')"
  ))

  expect_message(
    results <- fit_models(
      formulas = mixed_formulas,
      data = dengue_MS,
      family = "nbinomial",
      name = "Mixed",
      offset_var = "pop100000",
      nthreads = 2,
      control_compute = list(config = FALSE),
      progress = FALSE
    ),
    "Skipping Model 2"
  )

  expect_equal(nrow(results$mod_gof), 2)
  expect_true(is.na(results$mod_gof$dic[2]))
  expect_true(is.data.frame(results$fitted[[1]]))
  expect_null(results$fitted[[2]])
  expect_null(results$fixed[[2]])
  expect_null(results$random[[2]])
})

test_that("fit_models does not corrupt a data column literally named 'offset'", {
  skip_if_not_installed("INLA")

  dengue_with_offset_col <- dengue_MS
  dengue_with_offset_col$offset <- rep(999, nrow(dengue_with_offset_col))

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_with_offset_col,
    family = "nbinomial",
    name = "OffsetCollision",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE),
    progress = FALSE
  )

  # The pre-existing 'offset' column must be untouched in the returned data
  expect_equal(results$data$offset, rep(999, nrow(dengue_with_offset_col)))
})

test_that("fit_models returns $vcov when control_compute$vcov = TRUE", {
  skip_if_not_installed("INLA")

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    name = "VcovTest",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE, vcov = TRUE),
    progress = FALSE
  )

  expect_false(is.null(results$vcov[[1]]))
  expect_false(is.null(results$vcov[[2]]))
})

test_that("fit_models returns NA (not NaN) for re_meanchange with no random effects", {
  skip_if_not_installed("INLA")

  no_re_formulas <- as_GHRformulas(c(
    "dengue_cases ~ tmin",
    "dengue_cases ~ pdsi"
  ))

  results <- fit_models(
    formulas = no_re_formulas,
    data = dengue_MS,
    family = "nbinomial",
    name = "NoRE",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE),
    progress = FALSE
  )

  expect_true(all(is.na(results$mod_gof$re_meanchange)))
  expect_false(any(is.nan(results$mod_gof$re_meanchange)))
})

test_that("fit_models labels models with a 'name' vector matching formula count", {
  skip_if_not_installed("INLA")

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    name = c("modelA", "modelB"),
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE),
    progress = FALSE
  )

  expect_equal(results$mod_gof$model_id, c("modelA", "modelB"))
  expect_equal(names(results$fitted), c("modelA", "modelB"))
})

test_that("print.GHRmodels prints a summary of the object", {
  skip_if_not_installed("INLA")

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    name = "PrintTest",
    offset_var = "pop100000",
    nthreads = 2,
    control_compute = list(config = FALSE),
    progress = FALSE
  )

  out <- capture.output(print(results))
  expect_true(any(grepl("GHRmodels object", out)))
  expect_true(any(grepl("Number of models:\\s*2", out)))
  expect_true(any(grepl("Outcome:\\s*dengue_cases", out)))
  expect_true(any(grepl("Family:\\s*nbinomial", out)))
})

test_that("fit_models errors when covariates are missing or contain NAs", {
  # Covariate not in data
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS[, setdiff(names(dengue_MS), "tmin")],
      family = "nbinomial"
    ),
    "have not been found in the data: tmin"
  )
  # NA in a linear covariate
  dat <- dengue_MS
  dat$tmin[1] <- NA
  expect_error(
    fit_models(formulas = ghr_forms, data = dat, family = "nbinomial"),
    "contain missing values: .*tmin"
  )
})

test_that("fit_models validates 'control_inla' and 'control_family'", {
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_inla = "adaptive"
    ),
    "'control_inla' must be a list"
  )
  expect_error(
    fit_models(
      formulas = ghr_forms,
      data = dengue_MS,
      family = "nbinomial",
      control_family = 1
    ),
    "'control_family' must be a list"
  )
})

test_that("fit_models forwards 'control_inla' and 'control_family' to the fitted models", {
  skip_if_not_installed("INLA")

  results <- fit_models(
    formulas = as_GHRformulas(formulas[1:2]),
    data = dengue_MS,
    family = "nbinomial",
    name = "Ctrl",
    offset_var = "pop100000",
    nthreads = 2,
    control_inla = list(strategy = "gaussian"),
    control_family = list(
      hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01)))
    ),
    keep_inlamod = TRUE,
    progress = FALSE
  )

  expect_length(results$inlamod, 2)
  for (m in results$inlamod) {
    expect_equal(m$.args$control.inla$strategy, "gaussian")
    expect_equal(
      as.character(m$.args$control.family[[1]]$hyper$theta$prior),
      "loggamma"
    )
  }
})
