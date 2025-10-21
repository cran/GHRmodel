
test_that("as_GHRformulas correctly processes basic formulas", {
  formulas <- c(
    "cases ~ tmin.l1 + pdsi.l1 + f(region, model='iid')",
    "cases ~ tmin.l2 + pdsi.l2 + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check class
  expect_s3_class(result, "GHRformulas")
  
  # Check output structure
  expect_true(is.list(result))
  expect_named(result, c("formulas", "vars", "re", "outcome"))
  
  # Verify extracted variables
  expect_equal(result$vars$covariate_1, c("tmin.l1", "tmin.l2"))
  expect_equal(result$vars$covariate_2, c("pdsi.l1", "pdsi.l2"))
  
  # Verify extracted random effects
  expect_equal(result$re, "f(region, model='iid')")
  
  # Check outcome variable
  expect_equal(result$outcome, "cases")
})

test_that("as_GHRformulas handles missing covariates (null model)", {
  formulas <- c(
    "cases ~ 1 + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check if no covariates are detected
  expect_equal(ncol(result$vars), 0)
  
  # Ensure random effects are extracted correctly
  expect_equal(result$re, "f(region, model='iid')")
  
  # Check outcome variable
  expect_equal(result$outcome, "cases")
})

test_that("as_GHRformulas correctly identifies non-linear transformations", {
  formulas <- c(
    "cases ~ f(INLA::inla.group(tmin.l1, method='cut', n=10), model='rw2') + f(region, model='iid')",
    "cases ~ f(INLA::inla.group(pdsi.l1, method='quantile', n=5), model='rw1') + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Ensure `split_inla_group()` correctly parses transformation labels
  expect_equal(result$vars$covariate_1, c("tmin.l1_nl_c10_rw2", "pdsi.l1_nl_q5_rw1"))
  
  # Verify extracted random effects
  expect_equal(result$re, "f(region, model='iid')")
})

test_that("as_GHRformulas correctly identifies interaction terms", {
  formulas <- c(
    "cases ~ tmin.l1 * pdsi.l1 + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Ensure interaction terms are split correctly
  expect_equal((result[["vars"]][[1]]), sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[1]))
  expect_equal((result[["vars"]][[2]]), sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[2]))
  expect_equal((result[["vars"]][[3]]), sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[3]))
  
  # Verify extracted random effects
  expect_equal(result$re, "f(region, model='iid')")
})

test_that("as_GHRformulas correctly extracts multiple random effects", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid') + f(time, model='rw1')",
    "cases ~ pdsi.l1 + f(region, model='iid') + f(time, model='rw1')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check random effects
  expect_length(result$re, 2)
  expect_true("f(region, model='iid')" %in% result$re)
  expect_true("f(time, model='rw1')" %in% result$re)
})

test_that("as_GHRformulas stops if formulas contain different outcome variables", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "other_cases ~ pdsi.l1 + f(region, model='iid')"
  )
  
  expect_error(as_GHRformulas(formulas), "Error: different outcomes identified across formulas")
})

test_that("as_GHRformulas stops if formulas have different random effects", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "cases ~ pdsi.l1 + f(region, model='bym2')"
  )
  
  expect_error(as_GHRformulas(formulas), "Error: different random effects identified across formulas")
})

test_that("as_GHRformulas stops if no formulas are provided", {
  expect_error(as_GHRformulas(character(0)), "No formulas provided.")
})

test_that("as_GHRformulas handles complex INLA formulas with replication", {
  formulas <- c(
    "cases ~ f(INLA::inla.group(tmin.l1, method='quantile', n=10), model='rw2', replicate=region) + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Ensure `split_inla_group()` correctly processes replication
  expect_equal(result$vars$covariate_1, "tmin.l1_nl_q10_rw2_rep_region")
  
  # Verify extracted random effects
  expect_equal(result$re, "f(region, model='iid')")
})

test_that("as_GHRformulas correctly extracts variables even with whitespace variations", {
  formulas <- c(
    "cases ~ 1 +   tmin.l1  +    f(region , model = 'iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Ensure spaces do not affect variable detection
  expect_equal(result$vars$covariate_1, "tmin.l1")
  
  # Verify extracted random effects
  expect_equal(result$re, "f(region , model = 'iid')")
})

test_that("as_GHRformulas correctly processes formulas with adjacency matrix", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='bym2', graph=g)"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Ensure adjacency matrix is included in the effect
  expect_equal(result$re, "f(region, model='bym2', graph=g)")
})

test_that("as_GHRformulas handles formulas with no spaces correctly", {
  formulas <- c(
    "cases~tmin.l1+pdsi.l1+f(region,model='iid')",
    "cases~tmin.l2+pdsi.l2+f(region,model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check covariates are correctly extracted even with no spaces
  expect_equal(result$vars$covariate_1, c("tmin.l1", "tmin.l2"))
  expect_equal(result$vars$covariate_2, c("pdsi.l1", "pdsi.l2"))
  
  # Ensure lack of spaces does not break random effect parsing
  expect_equal(result$re, "f(region,model='iid')")
  
  # Verify outcome variable is correctly extracted
  expect_equal(result$outcome, "cases")
})


test_that("as_GHRformulas correctly processes formulas with inconsistent spacing in INLA calls", {
  formulas <- c(
    "cases ~ f(INLA::inla.group( tmin.l1 , method = 'quantile' , n = 10 ), model = 'rw2' ) + f(region,model='iid')",
    "cases~ f(INLA::inla.group(pdsi.l1,method='cut',n=5),model='rw1') + f(region,model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check if `split_inla_group()` correctly extracts transformations despite spacing issues
  expect_equal(result$vars$covariate_1, c("tmin.l1_nl_q10_rw2", "pdsi.l1_nl_c5_rw1"))
  
  # Ensure random effect extraction remains correct
  expect_equal(result$re, "f(region,model='iid')")
  
  # Verify outcome consistency
  expect_equal(result$outcome, "cases")
})

test_that("as_GHRformulas correctly handles formulas with different numbers of covariates", {
  formulas <- c(
    "cases ~ tmin.l1 + pdsi.l1 + f(region, model='iid')",
    "cases ~ tmin.l2 + f(region, model='iid')",
    "cases ~ tmin.l3 + pdsi.l3 + urban + f(region, model='iid')",
    "cases ~ tmin.l4 + pdsi.l4 + urban + interaction_var + f(region, model='iid')",
    "cases ~ f(INLA::inla.group(tmin.l5, method='quantile', n=10), model='rw2') + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check that all formulas are correctly stored
  expect_length(result$formulas, 5)
  
  # Ensure outcome variable is correctly identified
  expect_equal(result$outcome, "cases")
  
  # Ensure the number of covariates is correctly stored
  expect_equal(nrow(result$vars), 5)  # 5 formulas
  expect_equal(ncol(result$vars), 4)  # Maximum of 4 covariates in one of the formulas
  
  # Check that each row corresponds to the correct covariates
  expect_equal(result$vars$covariate_1, c("tmin.l1", "tmin.l2", "tmin.l3", "tmin.l4", "tmin.l5_nl_q10_rw2"))
  expect_equal(result$vars$covariate_2, c("pdsi.l1", NA, "pdsi.l3", "pdsi.l4", NA))
  expect_equal(result$vars$covariate_3, c(NA, NA, "urban", "urban", NA))
  expect_equal(result$vars$covariate_4, c(NA, NA, NA, "interaction_var", NA))
  
  # Ensure random effect is correctly extracted for all formulas
  expect_equal(result$re, "f(region, model='iid')")
})


test_that("as_GHRformulas correctly handles Spatially Varying Coefficients", {
  formulas <- c(
    "cases ~ tmin.l1 + pdsi.l1 + f(region, model='iid') + f(meso_region, tmax.l1, moel = 'iid')",
    "cases ~ tmin.l2 + f(region, model='iid')",
    "cases ~ f(INLA::inla.group(tmin.l5, method='quantile', n=10), model='rw2') + f(region, model='iid')"
  )
  
  result <- as_GHRformulas(formulas)
  
  # Check that all formulas are correctly stored
  expect_length(result$formulas, 3)
  
  # Ensure outcome variable is correctly identified
  expect_equal(result$outcome, "cases")
  
  # Ensure the number of covariates is correctly stored
  expect_equal(nrow(result$vars), 3)  # 3 formulas
  expect_equal(ncol(result$vars), 3)  # Maximum of 3 covariates in one of the formulas
  
  # Check that each row corresponds to the correct covariates
  expect_equal(result$vars$covariate_1, c("tmin.l1", "tmin.l2", "tmin.l5_nl_q10_rw2"))
  expect_equal(result$vars$covariate_2, c("pdsi.l1", NA, NA))
  expect_equal(result$vars$covariate_3, c("f(meso_region, tmax.l1, moel = 'iid')", NA, NA))

  # Ensure random effect is correctly extracted for all formulas
  expect_equal(result$re, "f(region, model='iid')")
})
