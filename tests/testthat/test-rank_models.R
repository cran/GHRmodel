test_that("rank_models errors when given a non-GHRmodels object", {
  non_GHRmodels <- list(not_gof = data.frame(a = 1:3))
  expect_error(
    rank_models(non_GHRmodels),
    "`model` must be of class 'GHRmodels'"
  )
})

test_that("rank_models returns top models in ascending order of specified metric", {
  # Create a mock GHRmodels object
  models <- list(
    mod_gof = data.frame(
      model_id = c("mod1", "mod2", "mod3"),
      dic = c(10, 5, 7),
      waic = c(100, 50, 70),
      stringsAsFactors = FALSE
    )
  )
  class(models) <- "GHRmodels"
  
  # Check the default behavior (metric = "dic")
  # Should rank mod2 (dic = 5), mod3 (dic = 7), mod1 (dic = 10)
  result_default <- rank_models(models = models, n =3)
  expect_identical(result_default, c("mod2", "mod3","mod1"))
  
  # Check ranking by "waic"
  # Should rank mod2 (waic = 50), mod3 (waic = 70), mod1 (waic = 100)
  result_waic <- rank_models(models, metric = "waic", n = 3)
  expect_identical(result_waic, c("mod2", "mod3", "mod1"))
})

test_that("rank_models handles 'n' exceeding number of available models with a warning", {
  models <- list(
    mod_gof = data.frame(
      model_id = c("mod1", "mod2", "mod3"),
      dic = c(10, 5, 7),
      stringsAsFactors = FALSE
    )
  )
  class(models) <- "GHRmodels"
  
  # 'n' is larger than the total number of models, which is 3
  expect_warning(
    result_n_large <- rank_models(models, metric = "dic", n = 5),
    regexp = "exceeds the total number of models"
  )
  
  # All models should still be returned
  expect_identical(result_n_large, c("mod2", "mod3", "mod1"))
})

test_that("rank_models works correctly when there is exactly one model", {
  models <- list(
    mod_gof = data.frame(
      model_id = c("single_model"),
      dic = c(42),
      stringsAsFactors = FALSE
    )
  )
  class(models) <- "GHRmodels"
  
  result_single <- rank_models(models, metric = "dic", n = 1)
  expect_equal(result_single, "single_model")
})

