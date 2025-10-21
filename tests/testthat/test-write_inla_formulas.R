
test_that("write_inla_formulas generates correct formulas with fixed effects", {
  outcome_var <- "cases"
  covariates_list <- list(
    c("tmin.l1", "pdsi.l1"),
    c("tmin.l2", "pdsi.l2")
  )
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = TRUE
  )
  
  # Check that the number of formulas matches expected (baseline + 2)
  expect_length(formulas, 3)
  
  # Check baseline formula exists
  expect_true(any(grepl("cases ~ 1", formulas)))
  
  # Check correct covariate formulas are generated
  expect_true(any(grepl("cases ~ 1 \\+ tmin.l1 \\+ pdsi.l1", formulas)))
  expect_true(any(grepl("cases ~ 1 \\+ tmin.l2 \\+ pdsi.l2", formulas)))
})

test_that("write_inla_formulas handles multiple random effects", {
  outcome_var <- "disease_cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = TRUE,
    re1 = list(id = "time", model = "rw2"),
    re2 = list(id = "region", model = "bym2", graph = "adjacency_matrix")
  )
  
  # Check that the number of formulas matches expected (baseline + 1 covariate model)
  expect_length(formulas, 2)
  
  # Check that the formulas contain the random effects
  expect_true(any(grepl("time, model = 'rw2'.*", formulas)))
  expect_true(any(grepl("region, model = 'bym2', graph = adjacency_matrix*", formulas)))
})

test_that("write_inla_formulas correctly applies replicate option", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = TRUE,
    re1 = list(id = "time", model = "rw1", replicate = "year")
  )
  
  # Check that the formula contains the replicate argument
  expect_true(any(grepl("time, model = 'rw1', replicate = year*", formulas)))
})

test_that("write_inla_formulas correctly applies group option", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = TRUE,
    re1 = list(id = "region", model = "rw2", group = "country")
  )
  
  # Check that the formula contains the group argument
  expect_true(any(grepl("region, model = 'rw2', group = country*", formulas)))
})

test_that("write_inla_formulas correctly handles hyperparameters", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = TRUE,
    re1 = list(id = "region", model = "bym2", graph = "g", hyper = "prior_list")
  )
  
  # Check that the formula contains the hyperparameter argument
  expect_true(any(grepl("hyper = prior_list*", formulas)))
})

test_that("write_inla_formulas generates correct baseline model when requested", {
  outcome_var <- "cases"
  formulas <- write_inla_formulas(outcome = outcome_var, baseline = TRUE)
  
  # Check that a baseline model is included
  expect_length(formulas, 1)
  expect_true(grepl("cases ~ 1", formulas[[1]]))
})

test_that("write_inla_formulas returns correct formula without baseline model", {
  outcome_var <- "cases"
  covariates_list <- list(c("temperature", "humidity"))
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = covariates_list,
    baseline = FALSE
  )
  
  # Check that no baseline formula is included
  expect_length(formulas, 1)
  expect_false(any(grepl("cases ~ 1$", formulas)))
})

test_that("write_inla_formulas correctly handles missing outcome argument", {
  expect_error(write_inla_formulas(), "The 'outcome' argument is required but was not provided.")
})


test_that("write_inla_formulas throws error when only 're' is provided without 'id'", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  expect_error(
    write_inla_formulas(
      outcome = outcome_var,
      covariates = covariates_list,
      baseline = TRUE,
      re1 = list(model = "rw2")  # Missing `id`
    ),
    "'re' must include both 'id' and 'model'."
  )
})

test_that("write_inla_formulas throws error when only 'id' is provided without 're'", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  expect_error(
    write_inla_formulas(
      outcome = outcome_var,
      covariates = covariates_list,
      baseline = TRUE,
      re1 = list(id = "time")  # Missing `re`
    ),
    "'re' must include both 'id' and 'model'."
  )
})

### TEST: Hyperparameter should be a character
test_that("write_inla_formulas throws error when graph is missing and re %in% bym bym2", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  expect_error(
    write_inla_formulas(
      outcome = outcome_var,
      covariates = covariates_list,
      baseline = TRUE,
      re1 = list(id = "region", model = "bym2", hyper = list(0.01, 0.01))  # Hyper must be character
    ),
    "graph' is missing"
  )
})

test_that("write_inla_formulas throws error when hyper is not character", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  expect_error(
    write_inla_formulas(
      outcome = outcome_var,
      covariates = covariates_list,
      baseline = TRUE,
      re1 = list(id = "region", model = "bym2", graph = "g", hyper = list(0.01, 0.01))  # Hyper must be character
    ),
    "'hyper' must be a single character string representing the name of the hyperparameter list"
  )
})


test_that("write_inla_formulas throws error when graph is not a character", {
  outcome_var <- "cases"
  covariates_list <- list(c("tmin.l1", "pdsi.l1"))
  
  expect_error(
    write_inla_formulas(
      outcome = outcome_var,
      covariates = covariates_list,
      baseline = TRUE,
      re1 = list(id = "region", model = "bym2", graph = list(matrix(1, 2, 2)))  # Graph should be character
    ),
    "'graph' must be a single character string representing the adjacency matrix name."
  )
})

### TEST: No covariates, only random effects
test_that("write_inla_formulas correctly handles a model with only random effects (no fixed effects)", {
  outcome_var <- "cases"
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = NULL,  # No covariates provided
    baseline = TRUE,
    re1 = list(id = "time", model = "rw1")
  )
  
  # Check that the formula is correctly generated with only the random effect
  expect_length(formulas, 1)
})

### TEST for cyclic, scale and adjust.for.con.comp

### TEST: cyclic
test_that("write_inla_formulas correctly handles a model cyclic random effects)", {
  outcome_var <- "cases"
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = NULL,  # No covariates provided
    baseline = TRUE,
    re1 = list(id = "time", model = "rw1", cyclic = TRUE)
  )
  
  # Check that the formula is correctly generated with only the random effect
  expect_equal(formulas,"cases ~ 1 + f(time, model = 'rw1', cyclic = TRUE)" )
})

### TEST: scale.model
test_that("write_inla_formulas correctly handles a model scaled random effect variance)", {
  outcome_var <- "cases"
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = NULL,  # No covariates provided
    baseline = TRUE,
    re1 = list(id = "time", model = "rw1", scale.model = TRUE)
  )
  
  # Check that the formula is correctly generated with only the random effect
  expect_equal(formulas,"cases ~ 1 + f(time, model = 'rw1', scale.model = TRUE)" )
})

### TEST: adjust.for.con.comp
test_that("write_inla_formulas correctly handles a model with 
          adjust.for.con.comp random effect)", {
  outcome_var <- "cases"
  
  formulas <- write_inla_formulas(
    outcome = outcome_var,
    covariates = NULL,  # No covariates provided
    baseline = TRUE,
    re1 = list(id = "spat_id", model = "bym", graph = "g", 
               adjust.for.con.comp = TRUE)
  )
  
  # Check that the formula is correctly generated with only the random effect
  expect_equal(formulas,"cases ~ 1 + f(spat_id, model = 'bym', graph = g, adjust.for.con.comp = TRUE)" )
})

