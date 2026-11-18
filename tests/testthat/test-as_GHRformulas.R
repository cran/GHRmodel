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
  expect_equal(unname(result$re), "f(region, model='iid')")

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
  expect_equal(unname(result$re), "f(region, model='iid')")

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
  expect_equal(
    result$vars$covariate_1,
    c("tmin.l1_nl_c10_rw2", "pdsi.l1_nl_q5_rw1")
  )

  # Verify extracted random effects
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas correctly identifies interaction terms", {
  formulas <- c(
    "cases ~ tmin.l1 * pdsi.l1 + f(region, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  # Ensure interaction terms are split correctly
  expect_equal(
    (result[["vars"]][[1]]),
    sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[1])
  )
  expect_equal(
    (result[["vars"]][[2]]),
    sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[2])
  )
  expect_equal(
    (result[["vars"]][[3]]),
    sort(c("tmin.l1", "pdsi.l1", "tmin.l1:pdsi.l1")[3])
  )

  # Verify extracted random effects
  expect_equal(unname(result$re), "f(region, model='iid')")
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
  expect_equal(names(result$re), c("re_1", "re_2"))
})

test_that("as_GHRformulas 're' has a consistent shape regardless of count", {
  # No random effects: empty, but still a character vector (not a
  # data.frame), so downstream code never has to special-case its type.
  no_re <- as_GHRformulas("cases ~ tmin.l1")
  expect_type(no_re$re, "character")
  expect_length(no_re$re, 0)

  # One random effect: named "re_1", not left unnamed.
  one_re <- as_GHRformulas("cases ~ tmin.l1 + f(region, model='iid')")
  expect_type(one_re$re, "character")
  expect_equal(names(one_re$re), "re_1")

  # Two random effects: named "re_1", "re_2", consistently with the above.
  two_re <- as_GHRformulas(
    "cases ~ tmin.l1 + f(region, model='iid') + f(time, model='rw1')"
  )
  expect_type(two_re$re, "character")
  expect_equal(names(two_re$re), c("re_1", "re_2"))
})

test_that("as_GHRformulas stops if formulas contain different outcomes", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "other_cases ~ pdsi.l1 + f(region, model='iid')"
  )

  expect_error(
    as_GHRformulas(formulas),
    "Error: different outcomes identified across formulas"
  )
})

test_that("as_GHRformulas stops if formulas have different random effects", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "cases ~ pdsi.l1 + f(region, model='bym2')"
  )

  expect_error(
    as_GHRformulas(formulas),
    "Error: different random effects identified across formulas"
  )
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
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas extracts variables with whitespace variations", {
  formulas <- c(
    "cases ~ 1 +   tmin.l1  +    f(region , model = 'iid')"
  )

  result <- as_GHRformulas(formulas)

  # Ensure spaces do not affect variable detection
  expect_equal(result$vars$covariate_1, "tmin.l1")

  # Verify extracted random effects
  expect_equal(unname(result$re), "f(region , model = 'iid')")
})

test_that("as_GHRformulas correctly processes formulas with adjacency matrix", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='bym2', graph=g)"
  )

  result <- as_GHRformulas(formulas)

  # Ensure adjacency matrix is included in the effect
  expect_equal(unname(result$re), "f(region, model='bym2', graph=g)")
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
  expect_equal(unname(result$re), "f(region,model='iid')")

  # Verify outcome variable is correctly extracted
  expect_equal(result$outcome, "cases")
})


test_that("as_GHRformulas processes formulas with inconsistent spacing", {
  formulas <- c(
    "cases ~ f(INLA::inla.group( tmin.l1 , method = 'quantile' , n = 10 ), model = 'rw2' ) + f(region,model='iid')",
    "cases~ f(INLA::inla.group(pdsi.l1,method='cut',n=5),model='rw1') + f(region,model='iid')"
  )

  result <- as_GHRformulas(formulas)

  # Check `split_inla_group()` extracts transformations despite spacing issues
  expect_equal(
    result$vars$covariate_1,
    c("tmin.l1_nl_q10_rw2", "pdsi.l1_nl_c5_rw1")
  )

  # Ensure random effect extraction remains correct
  expect_equal(unname(result$re), "f(region,model='iid')")

  # Verify outcome consistency
  expect_equal(result$outcome, "cases")
})

test_that("as_GHRformulas handles formulas with different numbers of covs", {
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
  expect_equal(nrow(result$vars), 5) # 5 formulas
  expect_equal(ncol(result$vars), 4) # Maximum of 4 covariates

  # Check that each row corresponds to the correct covariates
  expect_equal(
    result$vars$covariate_1,
    c("tmin.l1", "tmin.l2", "tmin.l3", "tmin.l4", "tmin.l5_nl_q10_rw2")
  )
  expect_equal(
    result$vars$covariate_2,
    c("pdsi.l1", NA, "pdsi.l3", "pdsi.l4", NA)
  )
  expect_equal(result$vars$covariate_3, c(NA, NA, "urban", "urban", NA))
  expect_equal(result$vars$covariate_4, c(NA, NA, NA, "interaction_var", NA))

  # Ensure random effect is correctly extracted for all formulas
  expect_equal(unname(result$re), "f(region, model='iid')")
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
  expect_equal(nrow(result$vars), 3) # 3 formulas
  expect_equal(ncol(result$vars), 3) # Maximum of 3 covariates

  # Check that each row corresponds to the correct covariates
  expect_equal(
    result$vars$covariate_1,
    c("tmin.l1", "tmin.l2", "tmin.l5_nl_q10_rw2")
  )
  expect_equal(result$vars$covariate_2, c("pdsi.l1", NA, NA))
  expect_equal(
    result$vars$covariate_3,
    c("f(meso_region, tmax.l1, moel = 'iid')", NA, NA)
  )

  # Ensure random effect is correctly extracted for all formulas
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas keeps REs when 'model' isn't right after the id", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, replicate=grp, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  # The random effect must be preserved verbatim, not silently dropped
  expect_equal(unname(result$re), "f(region, replicate=grp, model='iid')")
  expect_equal(result$vars$covariate_1, "tmin.l1")
})

test_that("as_GHRformulas keeps random effects when 'hyper' precedes 'model'", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, hyper=prior_t, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(unname(result$re), "f(region, hyper=prior_t, model='iid')")
})

test_that("as_GHRformulas errors on a malformed f() term with no identifier", {
  formulas <- c(
    "cases ~ tmin.l1 + f(model='iid')"
  )

  expect_error(
    as_GHRformulas(formulas),
    "Random-effect term is missing an identifier variable"
  )
})

test_that("as_GHRformulas does not split on '+' nested inside a hyper spec", {
  formulas <- c(
    paste0(
      "cases ~ tmin.l1 + f(region, model='iid', ",
      "hyper=list(prec=list(prior='loggamma',param=c(1e+05,0.01))))"
    )
  )

  result <- as_GHRformulas(formulas)

  expect_equal(result$vars$covariate_1, "tmin.l1")
  expect_equal(
    unname(result$re),
    "f(region, model='iid', hyper=list(prec=list(prior='loggamma',param=c(1e+05,0.01))))"
  )
})

test_that("as_GHRformulas does not split on '+' inside a quoted string", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid', hyper=list(label='a+b'))"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(
    unname(result$re),
    "f(region, model='iid', hyper=list(label='a+b'))"
  )
})

test_that("as_GHRformulas handles a trailing '- 1' (no-intercept formula)", {
  formulas <- c(
    "cases ~ tmin.l1 - 1 + f(region, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  # The covariate must not be polluted with the trailing "- 1"
  expect_equal(result$vars$covariate_1, "tmin.l1")
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas handles a leading '-1' (no-intercept formula)", {
  formulas <- c(
    "cases ~ -1 + tmin.l1 + f(region, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(result$vars$covariate_1, "tmin.l1")
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas handles '-1' with no other covariates", {
  formulas <- c(
    "cases ~ f(region, model='iid') - 1"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(ncol(result$vars), 0)
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas requires random effects in the same order across formulas", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid') + f(time, model='rw1')",
    "cases ~ pdsi.l1 + f(time, model='rw1') + f(region, model='iid')"
  )

  # Same random effects, listed in a different order, are treated as
  # inconsistent by design: formulas fed to fit_models() must share an
  # identical random-effect structure, position for position.
  expect_error(
    as_GHRformulas(formulas),
    "Error: different random effects identified across formulas"
  )
})

test_that("as_GHRformulas errors when 'formulas' is not a character vector", {
  expect_error(
    as_GHRformulas(list("cases ~ tmin.l1")),
    "'formulas' must be a character vector"
  )
})

test_that("as_GHRformulas warns on duplicate formulas but keeps them", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "cases ~ tmin.l1 + f(region, model='iid')"
  )

  expect_warning(
    result <- as_GHRformulas(formulas),
    "Duplicate formulas detected"
  )
  # Duplicates are retained, not deduplicated
  expect_length(result$formulas, 2)
  expect_equal(result$formulas[1], result$formulas[2])
})

test_that("as_GHRformulas stops if a formula has the wrong number of '~'", {
  expect_error(
    as_GHRformulas("cases tmin.l1 + f(region, model='iid')"),
    "Each formula must contain exactly one '~'"
  )

  expect_error(
    as_GHRformulas("cases ~ tmin.l1 ~ f(region, model='iid')"),
    "Each formula must contain exactly one '~'"
  )
})

test_that("as_GHRformulas stops if formulas have a different number of REs", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid') + f(time, model='rw1')",
    "cases ~ pdsi.l1 + f(region, model='iid')"
  )

  expect_error(
    as_GHRformulas(formulas),
    "Error: different random effects identified across formulas"
  )
})

test_that("as_GHRformulas pads 'vars' when one formula has no covariates", {
  formulas <- c(
    "cases ~ 1 + f(region, model='iid')",
    "cases ~ tmin.l1 + pdsi.l1 + f(region, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(result$vars$covariate_1, c(NA, "tmin.l1"))
  expect_equal(result$vars$covariate_2, c(NA, "pdsi.l1"))
})

test_that("as_GHRformulas classifies SVC, non-linear, and RE terms together", {
  formulas <- c(paste0(
    "cases ~ tmin.l1 + ",
    "f(INLA::inla.group(pdsi.l1, method='quantile', n=10), model='rw2') + ",
    "f(meso_region, tmax.l1, model='iid') + ",
    "f(region, model='iid')"
  ))

  result <- as_GHRformulas(formulas)

  expect_equal(result$vars$covariate_1, "tmin.l1")
  expect_equal(result$vars$covariate_2, "pdsi.l1_nl_q10_rw2")
  expect_equal(
    result$vars$covariate_3,
    "f(meso_region, tmax.l1, model='iid')"
  )
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas treats f(unit, weights = cov) as an SVC", {
  # `weights =` is equivalent to the 2nd positional argument
  for (term in c(
    "f(unit, weights = cov, model='iid')",
    "f(unit, model='iid', weights = cov)",
    "f(weights = cov, unit, model='iid')"
  )) {
    result <- as_GHRformulas(paste(
      "cases ~ tmin.l1 + f(region, model='iid') +",
      term
    ))
    expect_equal(result$vars$covariate_2, term, info = term)
    expect_equal(unname(result$re), "f(region, model='iid')", info = term)
  }

  # A random effect that has no weights stays a random effect
  result <- as_GHRformulas("cases ~ tmin.l1 + f(region, model='iid')")
  expect_equal(unname(result$re), "f(region, model='iid')")
})

test_that("as_GHRformulas preserves the original formula text verbatim", {
  formulas <- c(
    "cases ~ tmin.l1 + f(region, model='iid')",
    "cases ~ pdsi.l1 + f(region, model='iid')"
  )

  result <- as_GHRformulas(formulas)

  expect_identical(result$formulas, formulas)
})

test_that("as_GHRformulas handles formulas written across multiple lines", {
  formulas <- c(
    "cases ~ 1 +
     tmin.l1 +
     f(region, model = 'iid', replicate = grp, hyper = prior_t) +
     f(time, model = 'rw1', hyper = prior_t)"
  )

  result <- as_GHRformulas(formulas)

  expect_equal(result$vars$covariate_1, "tmin.l1")
  expect_equal(
    unname(result$re),
    c(
      "f(region, model = 'iid', replicate = grp, hyper = prior_t)",
      "f(time, model = 'rw1', hyper = prior_t)"
    )
  )
})

test_that("as_GHRformulas errors clearly on an unparseable right-hand side", {
  expect_error(
    as_GHRformulas("cases ~ tmin.l1 + f(region, model='iid'"),
    "Could not parse formula right-hand side"
  )
})

test_that(".extract_formula_vars returns the variables of linear terms only", {
  ex <- GHRmodel:::.extract_formula_vars

  # Linear terms, interactions, transformations, offsets and intercepts
  expect_setequal(
    ex("y ~ a + b:c + d*e + I(g^2) + log(h) - 1"),
    c("a", "b", "c", "d", "e", "g", "h")
  )
  expect_equal(ex("y ~ -1 + a"), "a")
  expect_equal(ex("y ~ 1 + a"), "a")
  expect_equal(ex("y ~ offset(log(pop)) + a"), c("pop", "a"))

  # Repeated variables are returned once
  expect_equal(ex("y ~ a + a:b + b"), c("a", "b"))

  # f() terms (RE, SVC, non-linear) are ignored
  expect_equal(
    ex(paste0(
      "y ~ a + f(month, model='rw1') + f(unit, cov, model='iid') + ",
      "f(INLA::inla.group(tmin, n=10), model='rw2')"
    )),
    "a"
  )
  expect_length(ex("y ~ 1 + f(month, model='rw1')"), 0)
  expect_length(ex("y ~ f(month, model='rw1') - 1"), 0)

  # Errors
  expect_error(ex("y a"), "exactly one")
  expect_error(ex("y ~ a ~ b"), "exactly one")
  expect_error(ex("y ~ a + log("), "Could not parse")
})

test_that(".check_formula_vars checks linear covariates in the data", {
  chk <- GHRmodel:::.check_formula_vars
  df <- data.frame(y = 1:4, a = c(1, 2, 3, 4), b = c(0.1, 0.2, 0.3, 0.4))

  expect_silent(chk(c("y ~ a", "y ~ a + b:a + log(b)"), df))

  # Missing from the data, across all formulas
  expect_error(chk("y ~ nope", df), "not been found.*nope")
  msg <- tryCatch(
    chk(c("y ~ a + nope1", "y ~ b + nope2"), df),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "nope1")
  expect_match(msg, "nope2")

  # Missing values
  df_na <- df
  df_na$b[2] <- NA
  expect_error(chk("y ~ a + b", df_na), "missing values.*b")
  expect_silent(chk("y ~ a", df_na))

  # f() terms are not checked
  expect_silent(chk("y ~ a + f(nope, model='iid')", df))

  # Objects in the global environment (e.g., DLNM bases) are accepted, even
  # with NAs
  basis <- matrix(c(NA, 1, 2, 3, 4, 5, 6, 7), nrow = 4)
  assign(".test_basis", basis, envir = globalenv())
  on.exit(rm(".test_basis", envir = globalenv()), add = TRUE)
  expect_silent(chk("y ~ a + .test_basis", df))

  # Names that are neither in the data nor in the global environment still fail
  expect_error(chk("y ~ a + .test_basis_nope", df), "not been found")
})

test_that(".split_inla_group builds the same label for equivalent specs", {
  sp <- GHRmodel:::.split_inla_group

  # Standard form, with and without the INLA:: prefix
  expect_equal(
    sp("f(INLA::inla.group(tmin, method='quantile', n=10), model='rw2')"),
    "tmin_nl_q10_rw2"
  )
  expect_equal(
    sp("f(inla.group(tmin, method='quantile', n=10), model='rw2')"),
    "tmin_nl_q10_rw2"
  )

  # Variable given as x =, or positional but not first
  expect_equal(
    sp("f(INLA::inla.group(x = tmin, method='quantile', n=10), model='rw2')"),
    "tmin_nl_q10_rw2"
  )
  expect_equal(
    sp("f(INLA::inla.group(n=10, method='quantile', tmin), model='rw2')"),
    "tmin_nl_q10_rw2"
  )
  expect_equal(
    sp("f(INLA::inla.group(n=10, x=tmin, method='quantile'), model='rw2')"),
    "tmin_nl_q10_rw2"
  )

  # Positional n and method follow the formals order: (x, n, method)
  expect_equal(
    sp("f(INLA::inla.group(tmin, 10, 'cut'), model='rw2')"),
    "tmin_nl_c10_rw2"
  )

  # Argument order of n and method does not matter
  expect_equal(
    sp("f(INLA::inla.group(tmin, n=10, method='cut'), model='rw2')"),
    "tmin_nl_c10_rw2"
  )

  # Quote style and whitespace
  expect_equal(
    sp("f(inla.group(tmin, method=\"cut\", n=10), model=\"rw1\")"),
    "tmin_nl_c10_rw1"
  )
  expect_equal(
    sp(
      "f( INLA::inla.group( tmin , method = 'cut' , n = 10 ) , model = 'rw2' )"
    ),
    "tmin_nl_c10_rw2"
  )

  # Position of model / replicate in f(), and extra arguments around them
  expect_equal(
    sp("f(model='rw2', INLA::inla.group(tmin, n=10))"),
    "tmin_nl10_rw2"
  )
  expect_equal(
    sp("f(INLA::inla.group(tmin, n=10), replicate=urban, model='rw2')"),
    "tmin_nl10_rw2_rep_urban"
  )
  expect_equal(
    sp(paste0(
      "f(INLA::inla.group(tmin, n=10), ",
      "hyper=list(prec=list(prior='pc.prec', param=c(1, 0.01))), ",
      "model='rw2', cyclic=TRUE, replicate=urban)"
    )),
    "tmin_nl10_rw2_rep_urban"
  )

  # Expressions as variable or replicate
  expect_equal(
    sp("f(INLA::inla.group(log(tmin), n=10), model='rw2')"),
    "log(tmin)_nl10_rw2"
  )
  expect_equal(
    sp("f(INLA::inla.group(tmin, n=10), model='rw2', replicate=as.integer(u))"),
    "tmin_nl10_rw2_rep_as.integer(u)"
  )

  # Missing optional arguments
  expect_equal(sp("f(INLA::inla.group(tmin), model='rw2')"), "tmin_nl_rw2")
  expect_equal(sp("f(INLA::inla.group(tmin, n=10))"), "tmin_nl10")
  expect_equal(sp("f(INLA::inla.group(tmin))"), "tmin_nl")

  # Variable names ending in n, or containing "n"/"method"
  expect_equal(
    sp("f(INLA::inla.group(precip_n, n=5), model='rw1')"),
    "precip_n_nl5_rw1"
  )

  # Large n is not written in scientific notation
  expect_equal(
    sp("f(INLA::inla.group(tmin, n=100000), model='rw2')"),
    "tmin_nl100000_rw2"
  )

  # NA is passed through
  expect_equal(sp(NA_character_), NA_character_)

  # Non-literal arguments cannot be put in a label: error, not silent loss
  expect_error(
    sp("f(INLA::inla.group(tmin, n=nbins), model='rw2')"),
    "non-literal 'n'"
  )
  expect_error(
    sp("f(INLA::inla.group(tmin, n=10), model=mod)"),
    "non-literal 'model'"
  )
  expect_error(
    sp("f(INLA::inla.group(tmin, method=m, n=10), model='rw2')"),
    "non-literal 'method'"
  )
})

test_that(".join_inla_group inverts .split_inla_group", {
  sp <- GHRmodel:::.split_inla_group
  jn <- GHRmodel:::.join_inla_group

  expect_equal(
    jn("tmin_nl_q10_rw2"),
    "f(INLA::inla.group(tmin, method = 'quantile', n = 10), model = 'rw2')"
  )
  # n without method must not be read as the model
  expect_equal(
    jn("tmin_nl10_rw2"),
    "f(INLA::inla.group(tmin, n = 10), model = 'rw2')"
  )
  expect_equal(
    jn("tmin_nl_rw2_rep_urban"),
    "f(INLA::inla.group(tmin), model = 'rw2', replicate = urban)"
  )

  # Equivalent specifications round-trip to the same call
  specs <- c(
    "f(INLA::inla.group(x = tmin, n=10, method='cut'), model='rw2')",
    "f(INLA::inla.group(tmin, 10, 'cut'), model=\"rw2\")",
    "f(model='rw2', inla.group(n=10, tmin, method='cut'))"
  )
  expect_length(unique(jn(vapply(specs, sp, character(1)))), 1)
  expect_equal(
    jn(sp(specs[1])),
    "f(INLA::inla.group(tmin, method = 'cut', n = 10), model = 'rw2')"
  )

  # Labels without "_nl" are returned as is
  expect_equal(jn("tmin"), "tmin")
})
