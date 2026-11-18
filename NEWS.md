# GHRmodel 0.2.0

## New features

* New model selection workflow: `select_re()` and `select_fe()` perform
  two-stage (univariable then multivariable) selection of random-effects
  and fixed-effects specifications based on goodness-of-fit metrics.
  `select_report()` generates an HTML report summarising the results of
  either function. These functions are experimental and may change in
  future releases.
* New `predict_models()` function, a lighter-weight counterpart to
  `fit_models()` that fits multiple models and returns posterior
  predictive samples (or posterior mean/median, via the `pred_type`
  argument), without computing the full set of outputs and GoF metrics
  returned by `fit_models()`. Useful for cross-validation in **GHRpredict**.
* `fit_models()` and `predict_models()` now support two additional
  likelihood families: `"gaussian"` and zero-inflated count models
  (`"zeroinflatedpoisson0"`, `"zeroinflatednbinomial0"`).
* `fit_models()` gains `keep_ppd` and `keep_inlamod` arguments to
  optionally retain, respectively, the posterior predictive samples and
  the fitted `INLA` model objects in the returned `GHRmodels` object.
* `fit_models()` and `predict_models()` now accept plain character
  vectors of formulas directly (in addition to `GHRformulas` objects).
* `fit_models()` and `predict_models()` gain the arguments `control_inla`
  and `control_family`, which are passed to the `control.inla` and
  `control.family` arguments of `INLA::inla()` to customise the INLA
  inference strategy and the likelihood settings.
* New `print()` method for `GHRmodels` objects.
* `plot_coef_crosspred()` gains a `heat_lims` argument to fix the colour
  scale limits of crossbasis heatmaps.
* The example datasets `dengue_MS` and `dengue_SP` now include a
  population column (`pop100000`, population divided by 100,000) for use
  with the `offset_var` argument of `fit_models()`/`predict_models()`.
* New exported function `crps_empirical()`, an efficient estimator of
  the empirical continuous ranked probability score, which is used to
  compute the CRPS in `fit_models()`.
* `plot_fit()` gains the arguments `unit_id` (to show only some spatial
  units) and `aggregate_space` (to aggregate fitted and observed values
  over a larger spatial unit).
* `plot_gof()` gains the arguments `title`, `xlab`, `ylab` and `legend`.

## Breaking changes

* In `fit_models()` the `offset` argument is deprecated and replaced by
  `offset_var`. The offset variable is now transformed internally
  according to the family (log for count families, identity for
  `"gaussian"`).
* In `fit_models()` the `pb` argument is replaced by `progress`, and a
  new `verbose` argument runs `INLA` in verbose mode.
* In `plot_re()` the `map_area` argument is renamed `map_unit`.
* In `plot_fit()` the arguments `group` and `group_id` are renamed
  `unit` and `unit_id`, and `mod_facet` is removed.
* `sample_ppd()` is no longer exported; use `predict_models()` instead.
* `crosspred_inla()` no longer accepts `...`.

## Improvements

* `stack_models()` now also merges the `$re`, `$inlamod` and `$ppd`
  components of `GHRmodels` objects, and validates that all inputs
  share the same family, outcome and outcome values.
* The CRPS is now the median across observations of the empirical CRPS
  of the posterior predictive distribution, and the CRPS skill score
  (`crpss`) relative to the first model is reported in `mod_gof`.
* `fit_models()`, `predict_models()` and the formula helpers have
  stricter input checks: the outcome and offset must be valid, and
  linear covariates used in the formulas must exist in `data` and have
  no missing values (e.g., lagged covariates must be computed on data
  that include the preceding time steps).
* MAE and RMSE goodness-of-fit metrics are now always computed as point
  estimates rather than averaged over posterior predictive samples.
* `fit_models()` no longer appends a numeric suffix to `name` when only
  a single model is fitted.
* Default `nthreads` in `fit_models()` changed from 8 to 1.
* Sampling from the posterior predictive distribution in `fit_models()`
  and `predict_models()` can now run in parallel, using up to `nthreads`
  threads.

## Bug fixes

* Fixed the handling of `nthreads` in `fit_models()` and
  `predict_models()`.
* Fixed a bug in the colour scale of crossbasis heatmaps.
* Fixed a bug in the temporal (single time series, no `unit`) case of
  the fixed/random-effects selection workflow.
* Fixed a bug in `plot_fit()`.
* Fixed a bug in `get_covariates()`.
* Fixed a bug in the parsing of non-linear terms (`inla.group()`) in
  `as_GHRformulas()`, which affected the names of non-linear variables.

## Documentation

* General documentation clean-up across functions and vignettes.


# GHRmodel 0.1.1

* Fixed bug in the colour scale in the function `plot_coef_crosspred()`.
* Minor updates in the documentation.


# GHRmodel 0.1.0

* Initial CRAN submission.
