# Load example data
data("dengue_MS", package = "GHRmodel")

# Filter to two years to keep model fitting fast
dengue_MS <- dengue_MS[dengue_MS$year %in% 2015:2016, ]

formulas <- c(
  "dengue_cases ~ tmin + f(month, model='rw1')",
  "dengue_cases ~ pdsi + f(month, model='rw2')"
)

test_that("predict_models validates required arguments", {
  expect_error(
    predict_models(
      formulas = 1,
      data = dengue_MS,
      family = "nbinomial"
    ),
    "'formulas' must be a character"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = as.matrix(dengue_MS),
      family = "nbinomial"
    ),
    "'data' must be a data.frame"
  )
})

test_that("predict_models validates 'family'", {
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "not_a_family"
    ),
    "Invalid 'family'"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = c("poisson", "nbinomial")
    ),
    "single character string"
  )
})

test_that("predict_models validates 'pred_type'", {
  # Regression test: previously 'pred_type' was never checked against its
  # allowed values because of a copy-pasted 'family' check, so an invalid
  # 'pred_type' silently slipped through the input checks.
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      pred_type = "not_a_method"
    ),
    "Invalid 'pred_type'"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      pred_type = c("ppd", "postmean")
    ),
    "single character string"
  )
})

test_that("predict_models validates 'offset_var', 'nthreads', 'idx', 'ppd_samples'", {
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      offset_var = "not_a_column"
    ),
    "'offset_var' must be a character string"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      offset = "pop100000"
    ),
    "'offset' has been deprecated, please use 'offset_var'"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      nthreads = -1
    ),
    "'nthreads' must be a single positive integer"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      idx = "a"
    ),
    "'idx' must be a NULL or a numeric vector"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      idx = nrow(dengue_MS) + 1
    ),
    "'idx' contains indices outside the valid range"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      ppd_samples = 1.5
    ),
    "'ppd_samples' must be a single positive integer"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      progress = c(TRUE, FALSE)
    ),
    "single logical value"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      verbose = "yes"
    ),
    "'verbose' must be a single logical value"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      verbose = c(TRUE, FALSE)
    ),
    "'verbose' must be a single logical value"
  )
})

test_that("predict_models errors when the offset variable contains NAs", {
  dat <- dengue_MS
  dat$pop_na <- dat$pop100000
  dat$pop_na[1] <- NA
  expect_error(
    predict_models(
      formulas = formulas,
      data = dat,
      family = "nbinomial",
      offset_var = "pop_na"
    ),
    "The offset variable cannot contain missing values"
  )
})

test_that("predict_models errors when the offset variable is not numeric", {
  dat <- dengue_MS
  dat$pop_chr <- as.character(dat$pop100000)
  expect_error(
    predict_models(
      formulas = formulas,
      data = dat,
      family = "nbinomial",
      offset_var = "pop_chr"
    ),
    "The offset variable must be numeric"
  )
})

test_that("predict_models errors on offset values <= 0 for log-link families", {
  for (bad in c(0, -1)) {
    dat <- dengue_MS
    dat$pop_bad <- dat$pop100000
    dat$pop_bad[1] <- bad
    expect_error(
      predict_models(
        formulas = formulas,
        data = dat,
        family = "poisson",
        offset_var = "pop_bad"
      ),
      "Offset values <=0 have been detected"
    )
  }
})

test_that("predict_models errors when the outcome is not numeric", {
  dat <- dengue_MS
  dat$dengue_cases <- as.character(dat$dengue_cases)
  expect_error(
    predict_models(formulas = formulas, data = dat, family = "nbinomial"),
    "The outcome variable must be numeric"
  )
})

test_that("predict_models errors on non-count outcomes for count families", {
  for (bad in c(2.5, -2)) {
    dat <- dengue_MS
    dat$dengue_cases[1] <- bad
    for (fam in c("nbinomial", "poisson")) {
      expect_error(
        predict_models(formulas = formulas, data = dat, family = fam),
        "the outcome must consist of counts equal to or larger than 0"
      )
    }
  }
})

test_that("predict_models errors when the outcome is not in 'data'", {
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS[, setdiff(names(dengue_MS), "dengue_cases")],
      family = "nbinomial"
    ),
    "The outcome variable dengue_cases has not been found in the data"
  )
})

test_that("predict_models errors when formulas have different outcomes", {
  expect_error(
    predict_models(
      formulas = c("dengue_cases ~ tmin", "other_outcome ~ pdsi"),
      data = dengue_MS,
      family = "nbinomial"
    ),
    "All functions must share the same outcome"
  )
})

test_that("predict_models 'verbose' is applied to every fitted model", {
  skip_if_not_installed("INLA")

  # predict_models() doesn't return the fitted 'inla' objects, so intercept
  # '.fit_single_model()' (still delegating to the real implementation) to
  # record the 'verbose' value it was actually called with for each model.
  real_fit_single_model <- .fit_single_model
  verbose_calls <- list()
  testthat::local_mocked_bindings(
    .fit_single_model = function(...) {
      args <- list(...)
      verbose_calls[[length(verbose_calls) + 1]] <<- args$verbose
      real_fit_single_model(...)
    }
  )

  invisible(predict_models(
    formulas = formulas,
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    offset_var = "pop100000",
    progress = FALSE,
    verbose = TRUE
  ))

  expect_length(verbose_calls, length(formulas))
  expect_true(all(unlist(verbose_calls)))
})

test_that("predict_models forwards 'control_inla' and 'control_family' to every fitted model", {
  skip_if_not_installed("INLA")

  # Same interception as the 'verbose' test above: record the control
  # arguments '.fit_single_model()' was called with for each model.
  real_fit_single_model <- .fit_single_model
  inla_calls <- list()
  family_calls <- list()
  testthat::local_mocked_bindings(
    .fit_single_model = function(...) {
      args <- list(...)
      inla_calls[[length(inla_calls) + 1]] <<- args$control_inla
      family_calls[[length(family_calls) + 1]] <<- args$control_family
      real_fit_single_model(...)
    }
  )

  ctrl_inla <- list(strategy = "gaussian")
  ctrl_family <- list(
    hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01)))
  )
  invisible(predict_models(
    formulas = formulas,
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    offset_var = "pop100000",
    control_inla = ctrl_inla,
    control_family = ctrl_family,
    progress = FALSE
  ))

  expect_length(inla_calls, length(formulas))
  for (i in seq_along(formulas)) {
    expect_equal(inla_calls[[i]], ctrl_inla)
    expect_equal(family_calls[[i]], ctrl_family)
  }
})

test_that("predict_models with pred_type='ppd' returns a matrix per model with correct dims", {
  skip_if_not_installed("INLA")

  res <- predict_models(
    formulas = formulas,
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "ppd",
    name = "TestModel",
    offset_var = "pop100000",
    ppd_samples = 10,
    progress = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c("TestModel1", "TestModel2"))
  for (mod in res) {
    expect_true(is.matrix(mod))
    expect_equal(dim(mod), c(nrow(dengue_MS), 10L))
  }
})

test_that("predict_models with pred_type='postmean'/'postmedian' returns single-column matrices", {
  skip_if_not_installed("INLA")

  res_mean <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    name = "m",
    offset_var = "pop100000",
    progress = FALSE
  )
  expect_equal(dim(res_mean[["m"]]), c(nrow(dengue_MS), 1L))

  res_median <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmedian",
    name = "m",
    offset_var = "pop100000",
    progress = FALSE
  )
  expect_equal(dim(res_median[["m"]]), c(nrow(dengue_MS), 1L))

  # Values should differ between mean and median for a skewed count outcome
  expect_false(isTRUE(all.equal(res_mean[["m"]], res_median[["m"]])))
})

test_that("predict_models 'idx' subsets rows and preserves matrix shape, incl. length-1 idx", {
  skip_if_not_installed("INLA")

  sub_idx <- 10:20
  subset_res <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    name = "m",
    offset_var = "pop100000",
    idx = sub_idx,
    progress = FALSE
  )
  expect_equal(dim(subset_res[["m"]]), c(length(sub_idx), 1L))

  # Regression test: a single-row idx used to be silently transposed
  # (preds[idx, ] drops to a vector and as.matrix() turned it into a
  # ppd_samples x 1 column instead of a 1 x ppd_samples row).
  single_idx <- 5
  single_res <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    name = "m",
    offset_var = "pop100000",
    idx = single_idx,
    progress = FALSE
  )
  expect_equal(dim(single_res[["m"]]), c(1L, 1L))
})

test_that("predict_models 'ppd' respects 'idx' order and duplicates", {
  skip_if_not_installed("INLA")

  # INLA's 'selection' argument collapses duplicate indices and returns
  # them sorted ascending, so predict_models must remap rows back to the
  # requested 'idx' order/multiplicity itself. Rows 147/214 have very
  # different fitted means (~2723 vs ~6), so mixing up the order or
  # collapsing duplicates is easy to detect.
  dup_idx <- c(214, 214, 147)
  dup_res <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "ppd",
    name = "m",
    offset_var = "pop100000",
    idx = dup_idx,
    ppd_samples = 10,
    progress = FALSE
  )
  expect_equal(dim(dup_res[["m"]]), c(length(dup_idx), 10L))
  expect_identical(dup_res[["m"]][1, ], dup_res[["m"]][2, ])

  unsorted_idx <- c(147, 214, 262)
  ppd_res <- predict_models(
    formulas = formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "ppd",
    name = "m",
    offset_var = "pop100000",
    idx = unsorted_idx,
    ppd_samples = 200,
    progress = FALSE
  )
  expect_equal(dim(ppd_res[["m"]]), c(length(unsorted_idx), 200L))
  # Row order must follow 'idx' (high, low, mid), not ascending index order
  # (147, 214, 262), which would also happen to sort the rows low-to-high.
  row_means <- rowMeans(ppd_res[["m"]])
  expect_true(row_means[1] > row_means[3])
  expect_true(row_means[3] > row_means[2])
})

test_that("predict_models default 'name' produces suffixed ids for multiple formulas and a bare id for one", {
  skip_if_not_installed("INLA")

  # Naming (names(modout) <- model_ids) is assigned before any model is
  # fitted and never touched again afterwards regardless of fit outcome
  # (see the "skips models that fail to fit" test below), so
  # guaranteed-to-fail formulas are enough to test it without paying for a
  # real INLA fit.
  bad_formulas <- c(
    "dengue_cases ~ log(tmin, base = 'a')",
    "dengue_cases ~ log(pdsi, base = 'a')"
  )

  res_multi <- suppressMessages(predict_models(
    formulas = bad_formulas,
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    offset_var = "pop100000",
    progress = FALSE
  ))
  expect_named(res_multi, c("mod1", "mod2"))

  res_single <- suppressMessages(predict_models(
    formulas = bad_formulas[1],
    data = dengue_MS,
    family = "nbinomial",
    pred_type = "postmean",
    offset_var = "pop100000",
    progress = FALSE
  ))
  expect_named(res_single, "mod")
})

test_that("predict_models skips models that fail to fit and keeps others", {
  skip_if_not_installed("INLA")

  bad_formulas <- c(
    formulas[1],
    "dengue_cases ~ log(tmin, base = 'a')"
  )

  # A single call covers both the message and the returned structure,
  # instead of fitting the same models twice.
  expect_message(
    res <- predict_models(
      formulas = bad_formulas,
      data = dengue_MS,
      family = "nbinomial",
      pred_type = "postmean",
      name = "m",
      offset_var = "pop100000",
      progress = FALSE
    ),
    "Skipping Model 2"
  )

  expect_length(res, 2)
  expect_false(is.null(res[["m1"]]))
  expect_null(res[["m2"]])
})

test_that("predict_models errors when covariates are missing or contain NAs", {
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS[, setdiff(names(dengue_MS), "tmin")],
      family = "nbinomial"
    ),
    "have not been found in the data: tmin"
  )
  dat <- dengue_MS
  dat$pdsi[1] <- NA
  expect_error(
    predict_models(formulas = formulas, data = dat, family = "nbinomial"),
    "contain missing values: pdsi"
  )
})

test_that("predict_models validates 'control_inla' and 'control_family'", {
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      control_inla = "adaptive"
    ),
    "'control_inla' must be a list"
  )
  expect_error(
    predict_models(
      formulas = formulas,
      data = dengue_MS,
      family = "nbinomial",
      control_family = 1
    ),
    "'control_family' must be a list"
  )
})
