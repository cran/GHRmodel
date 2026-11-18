#_______________________________________________________________________________
# Helper Functions for Internal Checks ----------------------------------------
#_______________________________________________________________________________
#' Fit a Single INLA Model
#'
#' This function fits a single Bayesian model using the \code{R-INLA} package.
#' It is used internally by \code{\link{fit_models}} but can be called directly
#' to fit and inspect a single model at a time.
#'
#' @param formula A \strong{formula} or character string specifying the model to
#' be fitted. If a character string is provided, it will be converted to a
#' formula via \code{stats::as.formula()}.
#' @param data A \strong{data.frame} containing all variables referenced in
#' \code{formula}.
#' @param family A character string specifying the likelihood family for the
#' INLA model (e.g., \code{"nbinomial"}, \code{"poisson"}, \code{"gaussian"}).
#' @param offset_values A numeric vector of offset values (same length as
#' \code{nrow(data)}) already transformed. If \code{NULL} (default), no
#' offset is applied.
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
#' @param nthreads Integer specifying the maximum number of threads for parallel
#' computation. Must be a positive integer. Defaults to \code{1}.
#'
#' @return A fitted \code{inla} model object (an S3 object of class
#' \code{inla}).
#'
#' @examples
#' \donttest{
#' # Create a simple dataset
#' my_data <- data.frame(
#'   y = rpois(100, lambda = 10),
#'   x = rnorm(100),
#'   offset_var = rep(1, 100)
#' )
#'
#' # Example formula
#' my_formula <- y ~ x
#'
#' # Fit a single Poisson model with offset
#' model_result <- .fit_single_model(
#'   formula = my_formula,
#'   data = my_data,
#'   family = "poisson",
#'   offset_values = my_data$offset_var,
#'   control_compute = list(config = TRUE, vcov = TRUE),
#'   nthreads = 2
#' )
#' }
#'
#' @seealso
#' \code{\link{fit_models}} for fitting multiple INLA models.
#' \code{\link{write_inla_formulas}} for writing INLA formulas.
#' \href{https://www.r-inla.org/}{R-INLA documentation}.
#'
#' @noRd

.fit_single_model <- function(
  formula,
  data,
  family,
  offset_values,
  control_compute = list(config = FALSE, vcov = FALSE),
  control_inla = list(strategy = "adaptive"),
  control_family = list(),
  nthreads = 1,
  verbose = FALSE
) {
  # INLA check
  if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    stop(
      "Package 'INLA' is required. Please install it from ",
      "https://inla.r-inla-download.org"
    )
  }

  # Arguments check
  if (missing(formula) || missing(data)) {
    stop("'formula' and 'data' must be provided")
  }

  # Extract config and vcov
  check_control_compute <- names(control_compute)

  if (!("config" %in% check_control_compute)) {
    control_compute$config <- FALSE
  }

  if (!("vcov" %in% check_control_compute)) {
    control_compute$vcov <- FALSE
  }

  config <- control_compute$config
  vcov <- control_compute$vcov

  # Convert 'forms' to formula if it's a character string
  if (is.character(formula)) {
    formula <- tryCatch(
      stats::as.formula(formula),
      error = function(e) {
        stop(
          "'form' could not be converted to a valid formula. ",
          "Please check the syntax."
        )
      }
    )
  }

  #Fit model
  INLA::inla(
    formula = formula,
    family = family,
    offset = offset_values,
    control.inla = control_inla,
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = config
    ),
    control.family = control_family,
    control.predictor = list(link = 1, compute = TRUE),
    verbose = verbose,
    num.threads = nthreads,
    control.fixed = list(correlation.matrix = vcov),
    data = data
  )
}

#' Extract Goodness-of-Fit Metrics from a Fitted INLA Model
#'
#' This function extracts various goodness-of-fit (GoF) metrics from
#' a fitted \code{INLA} model object. It is primarily intended for internal
#' use by \code{\link{fit_models}}, but can also be called independently if
#' desired.
#'
#' @param fitted_model An \code{inla} model object representing the fitted
#' model.
#' @param data A \code{data.frame} containing the dataset used to fit the model.
#' @param outcome A character string specifying the name of the outcome variable
#' in \code{data}.
#' @param config Logical; \code{config = TRUE} enables sampling from the
#' posterior predictive distribution (needed for posterior predictive checks
#' like the **CRPS** metric). Defaults to \code{FALSE}.
#' @param ci Logical; if \code{TRUE}, confidence intervals for DIC and WAIC are
#' computed. Defaults to \code{TRUE}.
#' @param family A character string specifying the likelihood family (e.g.,
#' \code{"nbinomial"}, \code{"poisson"}). Defaults to \code{"nbinomial"}.
#' @param no_re Logical; if \code{TRUE}, random effect variances are not
#' extracted. Defaults to \code{TRUE}.
#' @param formula (Optional) A \code{GHRformulas} or similar object containing
#' model formulas. Required if \code{no_re = FALSE} to identify random effects
#' for variance extraction.
#' @param ppd_samples Numeric indicating the number of posterior predictive
#' samples to draw. Only used if `config=TRUE`, defaults to 500.
#' @param nthreads Integer specifying the maximum number of threads used to
#' sample from the posterior predictive distribution. Only used if
#' `config=TRUE`. Defaults to 1.
#'
#' @details
#' This function is primarily used internally by \code{\link{fit_models}} to
#' gather GoF metrics across multiple fitted INLA models. However, it can also
#' be used directly if you have a single fitted model and want a structured
#'  summary of key GoF metrics.
#'
#' For details about the GoF metrics extracted by this function see
#' \code{\link{fit_models}} documentation.
#'
#' *Note*: If \code{no_re = FALSE}, random effect variances are extracted
#' from \code{fitted_model} using the model structure defined in a given
#' \code{GHRformula}.
#'
#' @return A list with one component, \code{mod_gof}, containing all extracted
#' goodness-of-fit metrics.
#'
#' @examples
#' \donttest{
#' # Create a simple dataset
#' data <- data.frame(
#'   y = rpois(100, lambda = 10),
#'   x = rnorm(100),
#'   time_id = seq(1, 100, by = 1),
#'   offset_var = rep(1, 100)
#' )
#'
#' # Example formula
#' formula <- y ~ x + f(time_id, model = 'rw1')
#'
#' # Fit a single Poisson model with offset
#' model_inla <- .fit_single_model(
#'   formula = formula,
#'   data = data,
#'   family = "poisson",
#'   offset_values = data$offset_var,
#'   nthreads = 2
#' )
#'
#' results <- .gof_single_model(
#'   fitted_model = model_inla,
#'   data = data,
#'   outcome = 'y',
#'   family = 'poisson',
#'   no_re = TRUE
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{fit_models}} for fitting multiple INLA models
#'
#' @noRd

.gof_single_model <- function(
  fitted_model,
  data,
  outcome,
  config = FALSE,
  ci = TRUE,
  family = "nbinomial",
  formula = NULL,
  no_re = TRUE,
  ppd_samples = 500,
  nthreads = 1
) {
  # Check required arguments
  if (missing(fitted_model) || missing(data) || missing(outcome)) {
    stop(
      "Error: one between 'fitted_models', 'data', 'outcome' ",
      "arguments not provided"
    )
  }

  # Check if no_re == FALSE GHRformulas are required
  if (no_re == FALSE && is.null(formula)) {
    stop(
      "'no_re' can be TRUE only if the corresponding GHRformulas ",
      "object is provided"
    )
  }

  # Extract information form the models depending if random effects exists
  if (no_re == TRUE) {
    modout <- list(
      mod_gof = dplyr::bind_cols(
        .extract_DIC_inla(model = fitted_model),
        .extract_WAIC_inla(model = fitted_model),
        .extract_LMS_inla(model = fitted_model)
      )
    )
  } else {
    modout <- list(
      mod_gof = dplyr::bind_cols(
        .extract_re_var_inla(model = fitted_model, formulas = formula),
        .extract_DIC_inla(model = fitted_model),
        .extract_WAIC_inla(model = fitted_model),
        .extract_LMS_inla(model = fitted_model)
      )
    )
  }

  # Sample from posterior predictive distribution and extract predictive metrics
  if (config == TRUE) {
    ppd <- .sample_ppd(
      fitted_model,
      family,
      ppd_samples,
      nthreads = nthreads
    )
    modout[["ppd"]] <- ppd

    mae_df <- .extract_MAE_ppd(ppd = ppd, data = data, cases = outcome)
    rmse_df <- .extract_RMSE_ppd(ppd = ppd, data = data, cases = outcome)
    crps <- .extract_CRPS_ppd(ppd = ppd, data = data, cases = outcome)

    modout[["mod_gof"]] <- modout[["mod_gof"]] |>
      dplyr::bind_cols(
        mae = mae_df$mae,
        rmse = rmse_df$rmse,
        crps = crps
      )
  } else {
    # Use summary.fitted.values when config = FALSE
    mae_df <- .extract_MAE_inla(
      model = fitted_model,
      data = data,
      cases = outcome
    )
    rmse_df <- .extract_RMSE_inla(
      model = fitted_model,
      data = data,
      cases = outcome
    )

    modout[["mod_gof"]] <- modout[["mod_gof"]] |>
      dplyr::bind_cols(
        mae = mae_df$mae,
        rmse = rmse_df$rmse,
        crps = NA
      )
  }

  # Return
  return(modout)
}


#' Validate Inputs Shared by fit_models() and predict_models()
#'
#' Runs the argument checks shared by \code{\link{fit_models}} and
#' \code{\link{predict_models}} (\code{data}, \code{family}, \code{name},
#' \code{offset}, \code{nthreads}, \code{progress}, \code{control_inla},
#' \code{control_family} and, optionally, \code{ppd_samples}). Checks specific to only one of the two functions
#' (e.g. \code{formulas}, \code{control_compute}, \code{pred_type},
#' \code{idx}, the offset values themselves) are left to the caller.
#'
#' @param data A data.frame.
#' @param family A character string; the likelihood family.
#' @param name Either a single character string or a character vector of
#' length \code{n_formulas}.
#' @param n_formulas The number of formulas being fit.
#' @param offset_var Either \code{NULL} or a character string naming a column
#' of \code{data}.
#' @param nthreads A single positive integer.
#' @param progress A single logical value.
#' @param verbose A single logical value.
#' @param ppd_samples Either \code{NULL} (skip the check, e.g. when it isn't
#' relevant for the current call) or a single non-negative integer to
#' validate.
#' @param outcome_var Outcome variable in data.
#' @param formulas A character vector of formulas whose variables are checked
#' against \code{data}.
#' @param offset Deprecated.
#' @param control_inla A list (passed to \code{control.inla} in INLA).
#' @param control_family A list (passed to \code{control.family} in INLA).
#'
#' @return Invisibly \code{NULL}. Called for its side effect of raising an
#' error on invalid input.
#'
#' @noRd
.check_inputs_models <- function(
  data,
  family,
  name,
  n_formulas,
  offset_var,
  nthreads,
  progress,
  verbose,
  ppd_samples = NULL,
  outcome_var,
  formulas,
  offset,
  control_inla,
  control_family
) {
  # data
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  # family
  if (!is.character(family) || length(family) != 1) {
    stop("'family' must be a single character string.")
  }
  if (
    !family %in%
      c(
        "nbinomial",
        "poisson",
        "gaussian",
        "zeroinflatedpoisson0",
        "zeroinflatednbinomial0"
      )
  ) {
    stop("Invalid 'family'.")
  }

  # name
  if (!is.character(name)) {
    stop("'name' must be a character.")
  }
  if (!length(name) %in% c(1, n_formulas)) {
    stop(
      "'name' must have either length 1 or the number ",
      "of provided formulas."
    )
  }
  if (anyDuplicated(name)) {
    stop("No duplicates in 'name' are allowed.")
  }

  # offset and offset_var
  if (!is.null(offset)) {
    stop("The argument 'offset' has been deprecated, please use 'offset_var'.")
  }
  if (
    !is.null(offset_var) &&
      (!is.character(offset_var) || !(offset_var %in% names(data)))
  ) {
    stop(
      "If provided, 'offset_var' must be a character ",
      "string that exists in 'data'."
    )
  }
  if (!is.null(offset_var) && !is.numeric(data[[offset_var]])) {
    stop("The offset variable must be numeric.")
  }
  if (!is.null(offset_var) && anyNA(data[[offset_var]])) {
    stop("The offset variable cannot contain missing values.")
  }

  # nthreads
  if (
    !is.numeric(nthreads) ||
      length(nthreads) != 1 ||
      nthreads <= 0 ||
      nthreads != as.integer(nthreads) ||
      is.na(nthreads)
  ) {
    stop("'nthreads' must be a single positive integer.")
  }

  # progress
  if (!is.logical(progress) || length(progress) != 1) {
    stop("'progress' must be a single logical value (TRUE or FALSE).")
  }

  # verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).")
  }

  # control_inla and control_family
  if (!is.list(control_inla)) {
    stop("'control_inla' must be a list.")
  }
  if (!is.list(control_family)) {
    stop("'control_family' must be a list.")
  }

  # ppd_samples (only checked when the caller opts in)
  if (!is.null(ppd_samples)) {
    if (
      length(ppd_samples) != 1 ||
        (ppd_samples %% 1) != 0 ||
        ppd_samples < 0 ||
        is.na(ppd_samples)
    ) {
      stop("'ppd_samples' must be a single positive integer.")
    }
  }

  # outcome_var
  if (!outcome_var %in% names(data)) {
    stop(paste0(
      "The outcome variable ",
      outcome_var,
      " has not been found in the data."
    ))
  }
  if (!is.numeric(data[[outcome_var]])) {
    stop("The outcome variable must be numeric.")
  }
  if (family != "gaussian") {
    y <- data[[outcome_var]]
    if (any(y %% 1 != 0 | y < 0, na.rm = TRUE)) {
      stop(paste0(
        "For family ",
        family,
        ", the outcome must consist of counts equal to or larger than 0."
      ))
    }
  }

  # variables used in the formulas
  .check_formula_vars(formulas, data)

  invisible(NULL)
}


#' Extract Per-Model Outputs From a Fitted INLA Model
#'
#' Gathers the outputs stored per model by \code{\link{fit_models}}
#' (\code{fitted}, \code{fixed}, \code{random}, \code{vcov}, \code{formulas},
#' \code{ppd}, \code{inlamod}) into a single named list. Used identically for
#' the base model and for each subsequent model fit in the loop.
#'
#' @param model A fitted \code{inla} model object.
#' @param gof The list returned by \code{.gof_single_model()} for \code{model}.
#' @param formula The (character) formula used to fit \code{model}.
#' @param config Logical; whether \code{control_compute$config} was \code{TRUE}.
#'
#' @return A named list with elements \code{fitted}, \code{fixed},
#' \code{random}, \code{vcov}, \code{formulas}, \code{ppd} and \code{inlamod}.
#' \code{vcov} and \code{ppd} are \code{NULL} when not available.
#'
#' @noRd
.extract_fitted <- function(model, gof, formula, config) {
  if (config && !is.null(gof[["ppd"]])) {
    ppd <- gof[["ppd"]]
    fitted <- data.frame(
      mean = apply(ppd, 1, mean),
      median = apply(ppd, 1, stats::median),
      lci = apply(ppd, 1, stats::quantile, probs = 0.025),
      uci = apply(ppd, 1, stats::quantile, probs = 0.975)
    )
  } else {
    ppd <- NULL
    fitted <- data.frame(
      mean = model[["summary.fitted.values"]][["mean"]],
      median = model[["summary.fitted.values"]][["0.5quant"]],
      lci = model[["summary.fitted.values"]][["0.025quant"]],
      uci = model[["summary.fitted.values"]][["0.975quant"]]
    )
  }

  list(
    fitted = fitted,
    fixed = model[["summary.fixed"]],
    random = model[["summary.random"]],
    vcov = model[["misc"]][["lincomb.derived.covariance.matrix"]],
    formulas = formula,
    ppd = ppd,
    inlamod = model
  )
}

#' Assign and potentially transform the offset values
#'
#' @param offset_var Either \code{NULL} or a character string naming a column
#' of \code{data}.
#' @param data A data.frame.
#' @param family A character string; the likelihood family. The numerical
#' offset values will be transformed for families other than `gaussian`.
#'
#' @return Either NULL or a (transformed) numeric vector
#'
#' @noRd
.process_offset <- function(offset_var, data, family) {
  if (is.null(offset_var)) {
    NULL
  } else {
    if (family != "gaussian") {
      if (any(data[[offset_var]] <= 0)) {
        stop(
          "Offset values <=0 have been detected for a family ",
          "that uses log as a link function. Please check."
        )
      }
      log(data[[offset_var]])
    } else {
      data[[offset_var]]
    }
  }
}

#' Print GHRmodels objects
#'
#' Generic print function for objects of class \code{GHRmodels}.
#' @param x An object of type \emph{GHRmodels}
#' @param ... other arguments.
#' @export
print.GHRmodels <- function(x, ...) {
  cat("GHRmodels object\n")
  cat("----------------\n")
  cat("Number of models: ", length(x$formulas), "\n")
  cat("Outcome: ", x$outcome, "\n")
  cat("Family: ", x$family, "\n")
  cat("Elements: ", paste(names(x), collapse = ", "), "\n")

  invisible(x)
}
