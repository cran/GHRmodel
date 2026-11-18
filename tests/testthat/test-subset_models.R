test_that("subset_models", {
  skip_if_not_installed("INLA")

  # Load example dataset
  data(dengue_MS)
  toydata <- dengue_MS[dengue_MS$year %in% 2010:2012, ]

  # Formulas
  formulas <- c(
    "dengue_cases ~ tmin + f(month, model='rw2')",
    "dengue_cases ~ pdsi + f(month, model='rw2')"
  )

  # Fit
  fitmod <- fit_models(
    formulas = formulas,
    data = toydata,
    family = "nbinomial",
    offset_var = "pop100000"
  )

  # Subset a single model
  sub1 <- subset_models(fitmod, mod_id = "mod1")
  expect_s3_class(sub1, "GHRmodels")
  expect_equal(length(sub1$fitted), 1)
  expect_equal(sub1$mod_gof$model_id, "mod1")

  # Subset multiple models
  sub_multi <- subset_models(fitmod, mod_id = c("mod1", "mod2"))
  expect_equal(length(sub_multi$fitted), 2)
  expect_setequal(sub_multi$mod_gof$model_id, c("mod1", "mod2"))

  # Rename
  sub_rename <- subset_models(
    fitmod,
    mod_id = c("mod1", "mod2"),
    new_name = "renamed_"
  )
  expect_s3_class(sub_rename, "GHRmodels")
  expect_equal(length(sub_rename$fitted), 2)
  expect_true(all(grepl("^renamed_", sub_rename$mod_gof$model_id)))
  expect_setequal(names(sub_rename$fitted), sub_rename$mod_gof$model_id)

  # Errors
  expect_error(
    subset_models(list(), mod_id = "mod1"),
    "must be an object of class 'GHRmodels'"
  )
  expect_error(
    subset_models(fitmod, mod_id = "mod999"),
    "None of the requested 'mod_id' values matched"
  )
  expect_error(
    subset_models(fitmod, mod_id = "mod1", new_name = c("a", "b")),
    "'new_name' must be a single character string or NULL"
  )
})
