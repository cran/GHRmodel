test_that("stack_models throws error when model_ids conflict and new_name is missing", {
  model1 <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  model2 <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
  
  expect_error(
    stack_models(model1, model2),
    "Some 'model_id's overlap across inputs"
  )
})

test_that("stack_models renames model_ids when new_name is provided", {
  model1 <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  model2 <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
  
  stacked <- stack_models(model1, model2, new_name = "model_")
  
  expect_s3_class(stacked, "GHRmodels")
  expect_equal(length(unique(stacked$mod_gof$model_id)), length(stacked$fixed))
  expect_true(all(names(stacked$fixed) == stacked$mod_gof$model_id))
})

test_that("stack_models merges two GHRmodels with unique model_ids", {
  model1 <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  model2 <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
  
  # Force unique model IDs 
  names(model2$fixed) <- paste0(names(model2$fixed), "_alt")
  model2$mod_gof$model_id <- names(model2$fixed)
  names(model2$fitted) <- names(model2$fixed)
  names(model2$random) <- names(model2$fixed)
  names(model2$vcov) <- names(model2$fixed)
  
  stacked <- stack_models(model1, model2)
  
  expect_s3_class(stacked, "GHRmodels")
  expect_true(length(stacked$fixed) == length(model1$fixed) + length(model2$fixed))
  expect_equal(nrow(stacked$mod_gof), length(stacked$fixed))
  expect_true(all(names(stacked$fixed) == stacked$mod_gof$model_id))
})

test_that("stack_models respects vs_first = FALSE by removing comparison cols", {
  model1 <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))
  model2 <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
  
  # Force unique model IDs
  names(model2$fixed) <- paste0(names(model2$fixed), "_alt")
  model2$mod_gof$model_id <- names(model2$fixed)
  names(model2$fitted) <- names(model2$fixed)
  names(model2$random) <- names(model2$fixed)
  names(model2$vcov) <- names(model2$fixed)
  
  stacked <- stack_models(model1, model2, vs_first = FALSE)
  
  expect_false(any(grepl("vs_first|r2dev|crpss", names(stacked$mod_gof))))
})

test_that("stack_models allows stacking list of GHRmodels", {
  model1 <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_t.rds"))
  model2 <- readRDS(testthat::test_path("testdata", "ghr_model_dlnm_vcov_f.rds"))
  
  # Ensure unique IDs
  names(model2$fixed) <- paste0(names(model2$fixed), "_v2")
  model2$mod_gof$model_id <- names(model2$fixed)
  
  stacked <- stack_models(list(model1, model2))
  
  expect_s3_class(stacked, "GHRmodels")
  expect_equal(length(stacked$fixed), length(model1$fixed) + length(model2$fixed))
})
