#' Fit Multiple INLA Models and sample their predicted vales
#'
#' This function is a simplified version of [fit_models] that only
#' keeps predictions for the input data and accepts formula strings with
#' different random effects.
#'
#' @param formulas A string vector containing multiple INLA model formulas.
#' Can be the `$formulas` component of a \code{GHRformulas} object.
#' @param data A data frame containing the variables used in the model formulas.
#' @param family A character string specifying the likelihood family. Currently
#' available options include \code{"poisson"}, \code{"nbinomial"},
#' \code{"gaussian"}, \code{"zeroinflatedpoisson0"}, and
#' \code{"zeroinflatednbinomial0"}.
#' @param pred_type A string indicating the prediction method to use. Options
#' include "ppd" (default), "postmean" and "postmedian". See details.
#' @param name Either a single character string to label each fitted model
#' by attaching a count (e.g., \code{"mod"}), or a character vector with the
#' the same number of \code{"formulas"} to label each of the models.
#' @param offset_var A character string specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param ppd_samples Numeric indicating the number of posterior predictive
#' samples to draw. Only used if `pred_type='ppd'`, defaults to 500.
#' @param idx Default is NULL (keep all data rows). Alternatively,
#' pass a vector of indices indicating the indices of the predictions to be
#' kept.
#' @param control_inla Parameters passed to `control.inla` in `INLA::inla`.
#' See `?INLA::control.inla` to see all available options. By default, uses
#' the adaptive strategy.
#' @param control_family Parameters passed to `control.family` in `INLA::inla`.
#' See `?INLA::control.family` to see all available options.
#' @param nthreads An integer specifying the maximum number of threads for
#' parallel computation. Default is \code{1}.
#' @param progress Logical; if \code{TRUE}, displays progression while fitting
#' models. Default is \code{TRUE}.
#' @param verbose Logical; if \code{TRUE}, run INLA in verbose mode (useful
#' for debugging). Default is \code{FALSE}.
#' @param offset Deprecated, use offset_var instead.
#'
#' @return A named list with model names according to `name`, each containing
#' a matrix with the predictions. The number of rows will be equal to the
#' number of rows of `data` (unless `idx` has been used) and the number of
#' columns will be equal to `ppd_samples` for `pred_type='ppd'` and 1
#' for the other prediction methods.
#'
#' @seealso [fit_models] for model fits with GoF metrics, INLA objects, and
#' other information about the model fit.
#'
#' @details The pred_type methods available include:
#' \itemize{
#'   \item \code{"ppd"}: Posterior predictive distribution, computed by
#'   generating posterior samples using `INLA::inla.posterior.sample` and
#'   simulating from the respective likelihood function.
#'   \item \code{"postmean"}: Posterior mean of the fitted mean response,
#'   obstained by `summary.fitted.values$mean` available in the INLA object.
#'   \item \code{"postmedian"}: Posterior median of the fitted mean response,
#'   obstained by `summary.fitted.values$0.5quant` available in the INLA object.
#' }
#' Note that the methods `postmean` and `postmedian` are faster than `pdd`,
#' but probabilistic scoring metrics cannot be calculated when using them.
#'
#' @examples
#' \donttest{
#' # Load example dataset
#' data(dengue_MS)
#' dengue_MS <- dengue_MS[dengue_MS$year %in% 2015:2016,]
#'
#' # Declare formulas
#' formula <- "dengue_cases ~ tmin + f(month, model='rw1')"
#'
#' # 10 ppd samples, keep rows 30 to 49
#' respred <- predict_models(
#'   formulas = formula,
#'   data = dengue_MS,
#'   family = "nbinomial",
#'   pred_type = "ppd",
#'   name = "TestModel",
#'   offset_var = "pop100000",
#'   ppd_samples = 10,
#'   idx = 30:49
#' )
#' }
#'
#' @export
predict_models <- function(
  formulas,
  data,
  family,
  pred_type = "ppd",
  name = "mod",
  offset_var = NULL,
  ppd_samples = 500,
  idx = NULL,
  control_inla = list(strategy = "adaptive"),
  control_family = list(),
  nthreads = 1,
  progress = TRUE,
  verbose = FALSE,
  offset = NULL
) {
  # 1) Checks -----

  # Required argument: formulas
  if (!is.character(formulas)) {
    stop("'formulas' must be a character.")
  }

  # Required argument: pred_type
  if (!is.character(pred_type) || length(pred_type) != 1) {
    stop("'pred_type' must be a single character string.")
  }
  if (!pred_type %in% c("ppd", "postmean", "postmedian")) {
    stop("Invalid 'pred_type'.")
  }

  # Check that the outcome is always the same
  all_outcomes <- character(length(formulas))
  for (i in seq_along(formulas)) {
    parts <- strsplit(formulas[i], "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      stop("Each formula must contain exactly one '~'.")
    }
    lhs <- trimws(parts[1])
    all_outcomes[i] <- lhs
  }
  outcome_var <- unique(all_outcomes)
  if (length(outcome_var) > 1) {
    stop("All functions must share the same outcome.")
  }

  # Shared checks (data, family, name, offset, nthreads, progress, ppd_samples)
  .check_inputs_models(
    data = data,
    family = family,
    name = name,
    n_formulas = length(formulas),
    offset_var = offset_var,
    offset = offset,
    nthreads = nthreads,
    progress = progress,
    verbose = verbose,
    ppd_samples = if (pred_type == "ppd") ppd_samples else NULL,
    outcome_var = outcome_var,
    formulas = formulas,
    control_inla = control_inla,
    control_family = control_family
  )

  # Assign the offset, transform if necessary
  offset_values <- .process_offset(offset_var, data, family)

  # 2) Extract arguments -----

  # idx
  if (is.null(idx)) {
    idx <- seq_len(nrow(data))
  } else if (!is.numeric(idx)) {
    stop("'idx' must be a NULL or a numeric vector.")
  } else {
    idx <- idx
    if (any(idx > nrow(data)) || any(idx < 0) || any(idx %% 1 != 0)) {
      stop("'idx' contains indices outside the valid range of the data.")
    }
  }

  # Number of formulas (models) and indicator for random effects
  n <- length(formulas)

  # Define names
  if (length(formulas) > 1) {
    if (length(name) == 1) {
      model_ids <- paste0(name, seq_along(formulas))
    } else {
      model_ids <- name
    }
  } else {
    model_ids <- name
  }

  # 3) Create object to store model outputs and set up progress bar -----
  modout <- vector("list", length = n)
  names(modout) <- model_ids

  # config = TRUE is only needed to draw ppd
  config_needed <- pred_type == "ppd"

  # 4) Iterate over models -----
  if (progress) {
    msg <- sprintf(
      "Models run: 0/%d",
      n
    )
    cat("\r", msg)
    utils::flush.console()
  }

  for (iteration in seq_along(formulas)) {
    out <- tryCatch(
      {
        # Fit model
        fitted_model <- .fit_single_model(
          formula = formulas[iteration],
          control_compute = list(config = config_needed, vcov = FALSE),
          control_inla = control_inla,
          control_family = control_family,
          family = family,
          data = data,
          offset_values = offset_values,
          nthreads = nthreads,
          verbose = verbose
        )

        # 5.1) ppd ----
        if (pred_type == "ppd") {
          preds <- .sample_ppd(
            fitted_model,
            family,
            ppd_samples,
            selection = list(Predictor = idx),
            nthreads = nthreads
          )
          # INLA's 'selection' returns unique indices in ascending order,
          # so remap rows back to the requested 'idx' order/multiplicity.
          sorted_unique_idx <- sort(unique(idx))
          modout[[iteration]] <- preds[
            match(idx, sorted_unique_idx),
            ,
            drop = FALSE
          ]
        } else if (pred_type == "postmean") {
          # 5.2) mean point estimate of the posterior mean fitted values ----
          preds <- matrix(
            fitted_model$summary.fitted.values[, "mean"],
            ncol = 1
          )
          modout[[iteration]] <- preds[idx, , drop = FALSE]
        } else if (pred_type == "postmedian") {
          # 5.3) median point estimate of the posterior mean fitted values ----
          preds <- matrix(
            fitted_model$summary.fitted.values[, "0.5quant"],
            ncol = 1
          )
          modout[[iteration]] <- preds[idx, , drop = FALSE]
        }
      },
      error = function(e) {
        message(sprintf(
          "INLA returned an error, model failed to run. Skipping Model %d.",
          iteration
        ))
        modout[[iteration]] <- NULL
      }
    )

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

  # 7) Return output
  if (progress) {
    cat("\n")
  }

  return(modout)
}
