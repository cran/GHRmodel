test_that("stack_models errors with different data, distribution, or outcome", {
  skip_if_not_installed("INLA")

  # Load example dataset
  data(dengue_MS)
  toydata <- dengue_MS[dengue_MS$year %in% 2010:2012, ]

  # Formulas
  formulas <- "dengue_cases ~ tmin + f(month, model='rw2')"

  # Fit
  fitmod1 <- fit_models(
    formulas = formulas,
    data = toydata,
    family = "nbinomial",
    offset_var = "pop100000"
  )

  # Sucess - rename
  fitmod2 <- fitmod1
  stacked <- stack_models(fitmod1, fitmod2, new_name = "mod")
  expect_s3_class(stacked, "GHRmodels")
  expect_equal(length(unique(stacked$mod_gof$model_id)), length(stacked$fixed))
  expect_true(all(names(stacked$fixed) == stacked$mod_gof$model_id))

  # Sucess - vs_first FALSE
  fitmod2 <- fitmod1
  stacked <- stack_models(fitmod1, fitmod2, new_name = "mod", vs_first = FALSE)
  expect_false(any(grepl("vs_first|r2dev|crpss", names(stacked$mod_gof))))

  # Errors - family
  fitmod2 <- fitmod1
  fitmod2$family <- "wrong"
  expect_error(
    stack_models(fitmod1, fitmod2),
    "Multiple families detected"
  )

  # Errors - outcome
  fitmod2 <- fitmod1
  fitmod2$outcome <- "wrong"
  expect_error(
    stack_models(fitmod1, fitmod2),
    "Multiple outcomes detected"
  )

  # Errors - family
  fitmod2 <- fitmod1
  fitmod2$data$dengue_cases[10] <- 50
  expect_error(
    stack_models(fitmod1, fitmod2),
    "Different outcome values detected"
  )

  # Errors - modelIDs
  fitmod2 <- fitmod1
  expect_error(
    stack_models(fitmod1, fitmod2),
    "Some 'model_id's overlap across inputs"
  )
})

test_that("stack_models keeps 're' as character when mixing no-RE and RE models", {
  # Minimal synthetic GHRmodels objects (no INLA/fitting needed) isolating
  # the 're' field, which as_GHRformulas() normalizes to character(0) when
  # a model has no random effects (rather than a 0-column data.frame).
  make_model <- function(id, re) {
    structure(
      list(
        mod_gof = data.frame(model_id = id, dic = 1, stringsAsFactors = FALSE),
        fitted = stats::setNames(list(data.frame()), id),
        fixed = stats::setNames(list(data.frame()), id),
        random = stats::setNames(list(list()), id),
        vcov = stats::setNames(list(NULL), id),
        formulas = stats::setNames("y ~ 1", id),
        inlamod = stats::setNames(list(NULL), id),
        ppd = stats::setNames(list(NULL), id),
        family = "poisson",
        data = data.frame(y = 1:3),
        re = re,
        outcome = "y"
      ),
      class = c("GHRmodels", "list")
    )
  }

  no_re_model <- make_model("mod1", character(0))
  re_model <- make_model("mod2", c(re_1 = "f(region, model='iid')"))

  stacked <- stack_models(no_re_model, re_model)

  expect_type(stacked$re, "character")
  expect_equal(unname(stacked$re), "f(region, model='iid')")
})
