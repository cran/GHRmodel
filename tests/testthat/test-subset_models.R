test_that("subset_models correctly subsets one or more models", {
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  # Subset a single model
  sub1 <- subset_models(model_t, mod_id = "mod1")
  expect_s3_class(sub1, "GHRmodels")
  expect_equal(length(sub1$fitted), 1)
  expect_equal(sub1$mod_gof$model_id, "mod1")
  
  # Subset multiple models
  sub_multi <- subset_models(model_t, mod_id = c("mod1", "mod3", "mod5"))
  expect_equal(length(sub_multi$fitted), 3)
  expect_setequal(sub_multi$mod_gof$model_id, c("mod1", "mod3", "mod5"))
})

test_that("subset_models correctly renames model IDs when new_name is supplied", {
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  sub_named <- subset_models(model_t, mod_id = c("mod1", "mod2"), new_name = "renamed_")
  expect_s3_class(sub_named, "GHRmodels")
  expect_equal(length(sub_named$fitted), 2)
  expect_true(all(grepl("^renamed_", sub_named$mod_gof$model_id)))
  expect_setequal(names(sub_named$fitted), sub_named$mod_gof$model_id)
})

test_that("subset_models errors for invalid inputs", {
  model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  
  # No mod_id provided
  expect_error(subset_models(model_t), "The 'mod_id' argument is required")
  
  # Invalid class for models
  expect_error(subset_models(list(), mod_id = "mod1"), "must be an object of class 'GHRmodels'")
  
  # Nonexistent mod_id
  expect_error(subset_models(model_t, mod_id = "mod999"), "None of the requested 'mod_id' values matched")
  
  # Invalid new_name
  expect_error(subset_models(model_t, mod_id = "mod1", new_name = c("a", "b")),
               "'new_name' must be a single character string or NULL")
})
