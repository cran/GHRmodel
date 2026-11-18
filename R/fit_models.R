#' Fit Multiple INLA Models
#'
#' This function fits a set of \code{INLA} model formulas, provided in a
#' \code{GHRformulas} object, to a specified dataset. For each fitted model,
#' it extracts a range of outputs, including goodness-of-fit (GoF) metrics
#' and other model outputs (fitted values, fixed effects, random effects).
#' Results are extracted and stored in a `GHRmodels` object.
#'
#' @param formulas Either a \code{GHRformulas} object containing multiple
#' INLA model formulas, or a character vector of formulas (in this case,
#' `as_GHRformulas()` will be executed internally).
#' @param data A data frame containing the variables used in the model formulas.
#' @param family A character string specifying the likelihood family. Currently
#' available options include \code{"poisson"}, \code{"nbinomial"},
#' \code{"gaussian"}, \code{"zeroinflatedpoisson0"}, and
#' \code{"zeroinflatednbinomial0"}.
#' @param name Either a single character string to label each fitted model
#' by attaching a count (e.g., \code{"mod"}) if there is more than one formula,
#' or a character vector with the the same number of \code{"formulas"}.
#' @param offset_var A character string specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param control_compute A named list controlling additional computation
#' options passed to INLA's `control.compute`:
#'   \describe{
#'     \item{\code{config}}{Logical indicating whether to store extra internal
#'     configuration (e.g., enabling posterior predictive checks like CRPS).
#'     Defaults to \code{FALSE}.}
#'     \item{\code{vcov}}{Logical indicating whether to return the
#'     variance-covariance (correlation) matrix of fixed effects. Defaults to
#'     \code{FALSE}.}
#'   }
#' Note that other elements of `control.compute` cannot be modified.
#' @param control_inla Parameters passed to `control.inla` in `INLA::inla`.
#' See `?INLA::control.inla` to see all available options. By default, uses
#' the adaptive strategy.
#' @param control_family Parameters passed to `control.family` in `INLA::inla`.
#' See `?INLA::control.family` to see all available options.
#' @param ppd_samples Numeric indicating the number of posterior predictive
#' samples to draw. Only used if `config=TRUE`, defaults to 500.
#' @param keep_ppd Default is FALSE (do not keep ppd). If TRUE (requires
#' also `config=TRUE`), the posterior predictive distribution for all data
#' samples is saved in the object, which can be useful for `plot_ppd()`.
#' @param keep_inlamod Logical. Default is FALSE.
#' If TRUE, the fitted INLA model objects are stored in the output list,
#' which may result in a substantially larger object.
#' @param nthreads An integer specifying the maximum number of threads for
#' parallel computation. Default is \code{1}.
#' @param progress Logical; if \code{TRUE}, displays progression while fitting
#' models. Default is \code{FALSE}.
#' @param verbose Logical; if \code{TRUE}, run INLA in verbose mode (useful
#' for debugging). Default is \code{FALSE}.
#' @param offset Deprecated, use offset_var instead.
#'
#' @return An object of class \code{GHRmodels} containing:
#' \describe{
#'   \item{`$mod_gof`}{A data frame of model-specific goodness-of-fit metrics.}
#'   \item{`$fitted`}{A list of fitted values (one element per model).
#'   If \code{config = TRUE}, these are derived from the posterior predictive
#'   distribution (PPD); otherwise, they are extracted from INLA's
#'   \code{summary.fitted.values}.}
#'   \item{`$fixed`}{A list of summary tables for fixed effects (one element
#'   per model).}
#'   \item{`$random`}{A list of summary tables for random effects (one element
#'   per model).}
#'   \item{`$vcov`}{A list of variance-covariance (correlation) matrices of
#'   fixed effects (one element per model). Populated only when
#'   `control_compute$vcov = TRUE`.}
#'   \item{`$formulas`}{A character vector of the original model formulas used.}
#'   \item{`$re`}{A character vector specifying any random effects defined in
#'   \code{formulas}.}
#'   \item{`$outcome`}{A character string indicating the outcome variable used.}
#'   \item{`$family`}{A character string indicating the likelihood family used.}
#'   \item{`$data`}{The original data frame passed to the function.}
#'   \item{`$ppd`}{(if `keep_ppd=TRUE`) A list of ppd matrices where rows are
#'   observations in the same order as in `data` and columns are ppd samples.}
#'   \item{`$inlamod`}{(if `keep_inlamod=TRUE`) A list of inla model objects.}
#' }
#'
#' @details
#'
#' This function iterates over each formula in the \code{GHRformulas} object
#' and fits the corresponding \code{INLA} model using the internal function
#' \code{.fit_single_model()}. For each fitted model, it extracts the fitted
#' values, fixed effects, and random effects summaries. Then, it calculates a
#' series of model evaluation metrics using the \code{.gof_single_model()}
#' internal function.
#'
#' Because the first formula is used as the reference model for all
#' comparison metrics (see Section B below), if it fails to fit,
#' \code{fit_models()} stops and no results are returned for any model.
#' However, if a formula after the first fit fails, a message is
#' printed and that model is skipped (GoF row is filled with \code{NA}
#' and object components are left empty).
#'
#' The goodness-of-fit (GoF) metrics are organized into two categories:
#'
#' **A) Model-Specific Goodness-of-Fit Metrics**
#'
#' These are computed separately for each model:
#'
#' 1. **Deviance Information Criterion (DIC)**
#'    \deqn{DIC = \bar{D} + p_{D}}
#'    where \eqn{\bar{D}} is the posterior mean deviance and \eqn{p_{D}} is the
#'    effective number of parameters. Lower DIC values indicate a better model
#'    fit, balancing goodness-of-fit and model complexity.
#'
#' 2. **Watanabe-Akaike Information Criterion (WAIC)**
#'    \deqn{WAIC = -2\left(\mathrm{lppd} - p_{\mathrm{WAIC}}\right)}
#'    WAIC evaluates predictive accuracy and penalizes model complexity through
#'    the log pointwise predictive density (\eqn{\mathrm{lppd}}). Lower values
#'    imply better generalization.
#'
#' 3. **Log Mean Score (LMS)**
#'    \deqn{LMS = \frac{1}{n} \sum_{i=1}^n \left( -\log(\mathrm{CPO}_i) \right)}
#'    LMS assesses the average negative log-predictive density using Conditional
#'    Predictive Ordinates (CPO). Lower LMS values indicate stronger predictive
#'    performance by penalizing models that assign low probability to observed
#'    outcomes.
#'
#' 4. **Mean Absolute Error (MAE)**
#'    \deqn{MAE = \frac{1}{n} \sum_{i=1}^n \left| y_i - \hat{y}_i \right|}
#'    Average absolute difference between observed and a point estimate of
#'    the predictions. As a point estimate, it uses the
#'    \code{summary.fitted.values} provided by INLA when `config = FALSE` and
#'    the mean of the ppd if `config = TRUE`.
#'    MAE is more robust to outliers than RMSE. Lower MAE values indicate
#'    improved model fit.
#'
#' 5. **Root Mean Squared Error (RMSE)**
#'    \deqn{RMSE = \sqrt{ \frac{1}{n} \sum_{i=1}^n (y_i - \hat{y}_i)^2 }}
#'    Root average squared difference between observed and a point estimate of
#'    the predictions. As a point estimate, it uses the
#'    \code{summary.fitted.values} provided by INLA when `config = FALSE` and
#'    the mean of the ppd if `config = TRUE`.
#'    RMSE penalizes larger errors more heavily. Lower RMSE values reflect better
#'    model fit.
#'
#' 6. **Continuous Ranked Probability Score (CRPS)**
#'    \deqn{\mathrm{CRPS}(F, y) = \int_{-\infty}^{\infty} \left[F(t) - \mathbf{1}\{y \leq t\}\right]^2 dt}
#'    The intergral is approximated with the empirical CRPS (see [crps_empirical]):
#'    \deqn{\mathrm{CRPS} = \frac{1}{n} \sum_{i=1}^n |x_i - y| - \frac{1}{2n^2} \sum_{i=1}^n \sum_{j=1}^n |x_i - x_j|}
#'    CRPS assesses how well the entire posterior predictive distribution aligns
#'    with the observed outcome. The median CRPS across observations is
#'    returned. Lower scores suggest better calibrated predictive
#'    distributions. Only available when \code{config = TRUE}.
#'
#' **B) Model Comparison Metrics (relative to the first model)**
#'
#' The first model in the list is treated as the baseline for model comparisons.
#' All other models are evaluated against it using the following metrics:
#'
#' 7. **Difference in DIC and WAIC**
#'    Stored as \code{dic_vs_first} and \code{waic_vs_first}. These represent
#'    how much higher (or lower) each model's DIC/WAIC is compared to the first
#'    model. Additionally, 95% credible intervals for these differences are
#'    stored as \code{*_vs_first_lci} and \code{*_vs_first_uci}.
#'
#' 8. **Difference in MAE and RMSE**
#'    Stored as \code{mae_vs_first} and \code{rmse_vs_first}. These reflect the
#'    absolute difference in prediction error compared to the first model. No
#'    credible intervals are computed for these metrics.
#'
#' 9. **Continuous Ranked Probability Score Skill Score (CRPSS)**
#'    \deqn{\mathrm{CRPSS} = 1 - \frac{\mathrm{CRPS}_{\text{model}}}{\mathrm{CRPS}_{\text{baseline}}}}
#'    Indicates how much better the predictive distribution of the current
#'    model is relative to the baseline model. Values closer to 1 indicate
#'    improvement; negative values imply worse performance. Available only
#'    when \code{config = TRUE}.
#'
#' 10. **Pseudo R-squared based on deviance**
#'     \deqn{R^2 = 1 - \exp\left( \frac{-2}{n} \left( \frac{dev_{\text{model}}}{-2} - \frac{dev_{\text{base}}}{-2} \right) \right)}
#'     Captures relative deviance reduction compared to the baseline model.
#'     Values range from 0 (no improvement) to 1 (strong improvement).
#'
#' 11. **Random Effect Variance**
#'     \deqn{\mathrm{Var}_{re} = \frac{1}{\mathrm{precision}}}
#'     Quantifies residual variance due to group- or cluster-level effects.
#'     Computed only when random effects are defined in the model formula.
#'
#' 12. **Proportional Change in Random Effect Variance**
#'     \deqn{\frac{\mathrm{Var}_{re}}{\mathrm{Var}_{re}^{(1)}} - 1}
#'     Represents the relative change in group-level variance compared to
#'     the baseline model. Helps assess how much variance is explained by
#'     added covariates. `re1_var_change` refers to the change in specific
#'     random effects (ordered by number) whereas `re_meanchange` is the
#'     mean change across all random effects. `re_meanchange` is \code{NA}
#'     when no random effects are defined or if the
#'     variance change of any individual random effect is \code{NA}.
#'
#' @examples
#' \donttest{
#' # Load example dataset
#' data(dengue_MS)
#' dengue_MS <- dengue_MS[dengue_MS$year %in% 2010:2012, ]
#'
#' # Declare formulas
#' formulas <- c(
#'   "dengue_cases ~ tmin +  f(year, model='rw1')",
#'   "dengue_cases ~ pdsi +  f(year, model='rw1')"
#' )
#'
#' # Fit multiple models
#' results <- fit_models(
#'   formulas = formulas,
#'   data = dengue_MS,
#'   family = "nbinomial",
#'   name = "TestModel",
#'   offset_var = "pop100000",
#'   nthreads = 2,
#'   progress = TRUE
#' )
#'
#' # Inspect goodness-of-fit metrics
#' results$mod_gof
#'}
#'
#' @seealso
#' \code{\link{as_GHRformulas}} converts a set of R-INLA-compatible formulas
#' into a `GHRformulas` object.
#'
#' @export
#' @rdname fit_models
#' @name fit_models

fit_models <- function(
  formulas,
  data,
  family,
  name = "mod",
  offset_var = NULL,
  control_compute = list(config = FALSE, vcov = FALSE),
  control_inla = list(strategy = "adaptive"),
  control_family = list(),
  ppd_samples = 500,
  keep_ppd = FALSE,
  keep_inlamod = FALSE,
  nthreads = 1,
  progress = FALSE,
  verbose = FALSE,
  offset = NULL
) {
  # 1) Input checks ----

  # Required argument: formulas
  if (is.character(formulas)) {
    formulas <- as_GHRformulas(formulas)
  }
  if (!inherits(formulas, "GHRformulas")) {
    stop("'formulas' must be an object of class 'GHRformulas'.")
  }

  # Shared checks (data, family, name, offset, nthreads, progress, ppd_samples)
  .check_inputs_models(
    data = data,
    family = family,
    name = name,
    n_formulas = length(formulas[["formulas"]]),
    offset_var = offset_var,
    offset = offset,
    nthreads = nthreads,
    progress = progress,
    verbose = verbose,
    ppd_samples = ppd_samples,
    outcome_var = formulas$outcome,
    formulas = formulas[["formulas"]],
    control_inla = control_inla,
    control_family = control_family
  )

  # control_compute must be a named list with logicals
  if (!is.list(control_compute)) {
    stop("'control_compute' must be a named list.")
  }
  if (!all(names(control_compute) %in% c("config", "vcov"))) {
    stop(
      "'control_compute' list must only include names: 'config' and/or 'vcov'."
    )
  }
  if (
    "config" %in% names(control_compute) && !is.logical(control_compute$config)
  ) {
    stop("'config' in 'control_compute' must be a logical value.")
  }
  if ("vcov" %in% names(control_compute) && !is.logical(control_compute$vcov)) {
    stop("'vcov' in 'control_compute' must be a logical value.")
  }

  # Missing config and vcov options, extract
  if (!("config" %in% names(control_compute))) {
    control_compute$config <- FALSE
  }
  if (!("vcov" %in% names(control_compute))) {
    control_compute$vcov <- FALSE
  }
  config <- control_compute$config
  vcov_opt <- control_compute$vcov

  # Assign the offset, transform if necessary
  offset_values <- .process_offset(offset_var, data, family)

  # keep_ppd
  if (!is.logical(keep_ppd) || length(keep_ppd) != 1) {
    stop("'keep_ppd' must be a single logical value.")
  } else if (!isTRUE(control_compute$config) && isTRUE(keep_ppd)) {
    stop("'keep_ppd' can only be used when 'config=TRUE'.")
  }

  # keep_inlamod
  if (!is.logical(keep_inlamod)) {
    stop("'keep_inlamod' must be a single logical value.")
  }

  # 2) Prepare objects ----

  # Number of formulas (models) and indicator for random effects
  n <- length(formulas[["formulas"]])
  no_re <- length(formulas$re) == 0

  # Parameters for for 95% CI
  zscore <- stats::qnorm(0.975)
  n_obs <- nrow(data)

  # Object to store model outputs.
  modout <- list(
    mod_gof = data.frame(formulas[["vars"]]),
    fitted = vector("list", length = n),
    fixed = vector("list", length = n),
    random = vector("list", length = n),
    vcov = vector("list", length = n),
    formulas = rep(NA, times = n),
    inlamod = vector("list", length = n),
    ppd = vector("list", length = n),
    family = family
  )

  # We will store each model's GoF row in a list.
  gof_list <- vector("list", length = n)

  # 3) Model 1 (base model) ----
  # Run without tryCatch
  if (progress) {
    msg <- sprintf(
      "Models run: 0/%d",
      n
    )
    cat("\r", msg)
    utils::flush.console()
  }

  # Fit the base model
  model1 <- .fit_single_model(
    formula = formulas[["formulas"]][1],
    control_compute = list(config = config, vcov = vcov_opt),
    control_inla = control_inla,
    control_family = control_family,
    family = family,
    data = data,
    offset_values = offset_values,
    nthreads = nthreads,
    verbose = verbose
  )

  gof1 <- .gof_single_model(
    fitted_model = model1,
    data = data,
    outcome = formulas[["outcome"]],
    config = config,
    family = family,
    no_re = no_re,
    formula = formulas,
    ppd_samples = ppd_samples,
    nthreads = nthreads
  )

  # Append difference columns to model 1's GoF output.
  gof_row_1 <- gof1$mod_gof |>
    dplyr::bind_cols(
      dic_vs_first = 0,
      dic_vs_first_lci = 0,
      dic_vs_first_uci = 0,
      waic_vs_first = 0,
      waic_vs_first_lci = 0,
      waic_vs_first_uci = 0,
      mae_vs_first = 0,
      rmse_vs_first = 0,
      crpss = ifelse(config, 0, NA),
      r2dev = 0
    )

  # Capture the base model values for later comparison.
  base_values <- list(
    waic_base = gof_row_1[["waic"]],
    dic_base = gof_row_1[["dic"]],
    local_waic_base = model1[["waic"]][["local.waic"]],
    local_dic_base = model1[["dic"]][["local.dic"]],
    n_local = length(model1[["waic"]][["local.waic"]]),
    mae_base = gof_row_1[["mae"]],
    rmse_base = gof_row_1[["rmse"]],
    crps_base = gof_row_1[["crps"]],
    dev_base = model1$dic$deviance.mean
  )

  # Store GoF of model 1.
  gof_list[[1]] <- gof_row_1

  # Extract and store model 1's outputs
  extracted1 <- .extract_fitted(
    model = model1,
    gof = gof1,
    formula = formulas[["formulas"]][1],
    config = config
  )

  modout[["fitted"]][[1]] <- extracted1$fitted
  modout[["fixed"]][[1]] <- extracted1$fixed
  modout[["random"]][[1]] <- extracted1$random
  if (!is.null(extracted1$vcov)) {
    modout[["vcov"]][[1]] <- extracted1$vcov
  }
  modout[["formulas"]][1] <- extracted1$formulas

  if (isTRUE(keep_ppd)) {
    modout[["ppd"]][[1]] <- extracted1$ppd
  }

  if (isTRUE(keep_inlamod)) {
    modout[["inlamod"]][[1]] <- extracted1$inlamod
  }

  if (progress) {
    msg <- sprintf(
      "Models run: 1/%d",
      n
    )
    cat("\r", msg)
    utils::flush.console()
  }

  # 4) Loop across models 2:n  ----
  # with tryCatch

  # Create a default GoF row structure based on model 1 for use in error cases.
  default_gof <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(gof_row_1)))
  colnames(default_gof) <- colnames(gof_row_1)

  if (n >= 2) {
    for (iteration in 2:n) {
      out <- tryCatch(
        {
          model <- do.call(
            .fit_single_model,
            list(
              formula = formulas[["formulas"]][iteration],
              control_compute = list(config = config, vcov = vcov_opt),
              control_inla = control_inla,
              control_family = control_family,
              family = family,
              data = data,
              offset_values = offset_values,
              nthreads = nthreads,
              verbose = verbose
            )
          )

          gof_model <- .gof_single_model(
            fitted_model = model,
            data = data,
            outcome = formulas[["outcome"]],
            config = config,
            family = family,
            no_re = no_re,
            formula = formulas,
            ppd_samples = ppd_samples,
            nthreads = nthreads
          )

          waic_vs_first <- model[["waic"]][["waic"]] - base_values$waic_base
          dic_vs_first <- model[["dic"]][["dic"]] - base_values$dic_base
          local_waic_model <- model[["waic"]][["local.waic"]]
          local_dic_model <- model[["dic"]][["local.dic"]]
          dev_model <- model[["dic"]][["deviance.mean"]]

          waic_vs_first_se <- as.numeric(sqrt(
            base_values$n_local *
              stats::var(
                local_waic_model - base_values$local_waic_base,
                na.rm = TRUE
              )
          ))
          dic_vs_first_se <- as.numeric(sqrt(
            base_values$n_local *
              stats::var(
                local_dic_model - base_values$local_dic_base,
                na.rm = TRUE
              )
          ))

          # Gof Element
          gof_row <- gof_model$mod_gof |>
            dplyr::bind_cols(
              dic_vs_first = dic_vs_first,
              dic_vs_first_lci = dic_vs_first - dic_vs_first_se * zscore,
              dic_vs_first_uci = dic_vs_first + dic_vs_first_se * zscore,
              waic_vs_first = waic_vs_first,
              waic_vs_first_lci = waic_vs_first - waic_vs_first_se * zscore,
              waic_vs_first_uci = waic_vs_first + waic_vs_first_se * zscore,
              mae_vs_first = gof_model$mod_gof$mae - base_values$mae_base,
              rmse_vs_first = gof_model$mod_gof$rmse - base_values$rmse_base,
              crpss = ifelse(
                config,
                (1 - (gof_model$mod_gof$crps / base_values$crps_base)),
                NA
              ),
              r2dev = round(
                1 -
                  exp(
                    (-2 / n_obs) *
                      ((dev_model / -2) - (base_values$dev_base / -2))
                  ),
                3
              )
            )

          # Extract and store this model's outputs
          extracted <- .extract_fitted(
            model = model,
            gof = gof_model,
            formula = formulas[["formulas"]][iteration],
            config = config
          )

          modout[["fitted"]][[iteration]] <- extracted$fitted
          modout[["fixed"]][[iteration]] <- extracted$fixed
          modout[["random"]][[iteration]] <- extracted$random
          if (!is.null(extracted$vcov)) {
            modout[["vcov"]][[iteration]] <- extracted$vcov
          }

          # Store ppd for the 'iteration' model
          if (isTRUE(keep_ppd)) {
            modout[["ppd"]][[iteration]] <- extracted$ppd
          }

          # Store INLA model object for 'iteration' model
          if (isTRUE(keep_inlamod)) {
            modout[["inlamod"]][[iteration]] <- extracted$inlamod
          }

          gof_row
        },
        error = function(e) {
          message(sprintf(
            "INLA returned an error, model failed to run. Skipping Model %d.",
            iteration
          ))
          default_gof
        }
      )

      # Storing output
      gof_list[[iteration]] <- out
      modout[["formulas"]][iteration] <- formulas[["formulas"]][iteration]

      if (progress) {
        msg <- sprintf(
          "Models run: %d/%d",
          iteration,
          n
        )
        cat("\r", msg)
        utils::flush.console()
      }
    }
  }

  # 5) Postprocess and combine output ----

  # Combine the individual GoF rows into one data frame.
  mod_gof_all <- dplyr::bind_rows(gof_list)

  # Optionally compute changes across random effects.
  mod_gof_all <- mod_gof_all |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("_var"),
        function(x) (x - dplyr::first(x)) / dplyr::first(x),
        .names = "{.col}_change"
      )
    ) |>
    dplyr::mutate(
      re_meanchange = if (no_re) {
        NA_real_
      } else {
        rowMeans(dplyr::pick(tidyselect::matches("_var_change$")))
      }
    )

  # Attach a model ID to each model.
  if (nrow(mod_gof_all) > 1) {
    if (length(name) == 1) {
      mod_gof_all$model_id <- paste0(name, seq_len(nrow(mod_gof_all)))
    } else {
      mod_gof_all$model_id <- name
    }
  } else {
    mod_gof_all$model_id <- name
  }

  # Store the final combined GoF output directly.
  modout$mod_gof <- cbind(modout$mod_gof, mod_gof_all)

  # Relocate model_id and any "_var" columns + "re_meanchange"
  modout[["mod_gof"]] <- modout[["mod_gof"]] |>
    dplyr::relocate(all_of("model_id"), .before = 1) |>
    dplyr::relocate(
      tidyselect::contains("_var"),
      "re_meanchange",
      .after = dplyr::last_col()
    )

  # Name the lists according to the model IDs.
  names(modout[["formulas"]]) <- mod_gof_all$model_id
  names(modout[["fitted"]]) <- mod_gof_all$model_id
  names(modout[["fixed"]]) <- mod_gof_all$model_id
  names(modout[["random"]]) <- mod_gof_all$model_id
  names(modout[["vcov"]]) <- mod_gof_all$model_id

  if (isTRUE(keep_inlamod)) {
    names(modout[["inlamod"]]) <- mod_gof_all$model_id
  }
  if (isTRUE(keep_ppd)) {
    names(modout[["ppd"]]) <- mod_gof_all$model_id
  }

  modout[["data"]] <- data
  modout[["re"]] <- formulas[["re"]]
  modout[["outcome"]] <- formulas[["outcome"]]

  class(modout) <- c("GHRmodels", class(modout))

  # 6) Return ----
  if (progress) {
    cat("\n")
  }

  return(modout)
}
