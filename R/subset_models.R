#' @title Subset `GHRmodels` Objects
#' @description
#' This function subsets selected models from a `GHRmodels` object into a new
#' reduced `GHRmodels` object.
#'
#' @param models A \code{GHRmodels} object.
#' @param mod_id A character vector of model IDs indicating which model(s)
#'   to keep. These must match \code{models$mod_gof$model_id}.
#' @param new_name `NULL` (default) **or** a character used to build the new
#'   model IDs.
#'
#' @return A new \code{GHRmodels} object containing only the specified model(s).
#'
#' @seealso
#' \code{\link{stack_models}} for combining GHRmodels objects,
#' \code{\link{fit_models}} for fitting INLA models.
#'
#' @name subset_models
#' @rdname subset_models
#'
#' @examples
#' \donttest{
#' # Load example dataset
#'  data(dengue_MS)
#'  dengue_MS <- dengue_MS[dengue_MS$year %in% 2010:2012, ]
#'
#'  # Formulas
#'  formulas <- c(
#'    "dengue_cases ~ tmin + f(month, model='rw2')",
#'    "dengue_cases ~ pdsi + f(month, model='rw2')"
#'  )
#'
#'  # Fit
#'  fitmod <- fit_models(
#'    formulas = formulas,
#'    data = dengue_MS,
#'    family = "nbinomial",
#'    offset_var = "pop100000"
#'  )
#'
#'  # Subset a single model
#'  sub1 <- subset_models(fitmod, mod_id = "mod1")
#' }
#'
#' @export
#'

subset_models <- function(models, mod_id, new_name = NULL) {
  # 1) Checks

  # Check class of `model`
  if (!inherits(models, "GHRmodels")) {
    stop("'model' must be an object of class 'GHRmodels'.")
  }

  # Check that `mod_id` is provided
  if (missing(mod_id)) {
    stop(
      "The 'mod_id' argument is required to indicate which models to retain."
    )
  }

  # Check that `mod_id` is a character vector
  if (!is.character(mod_id)) {
    stop("'mod_id' must be a character vector.")
  }

  # Check that the 'model_id' column exists in model$mod_gof
  if (!"model_id" %in% names(models$mod_gof)) {
    stop("The model object does not contain a 'model_id' column in 'mod_gof'.")
  }

  # Check that at least one of the provided IDs exists
  all_mod_id <- models$mod_gof[["model_id"]]
  keep_idx <- which(all_mod_id %in% mod_id)

  if (length(keep_idx) == 0) {
    stop("None of the requested 'mod_id' values matched existing model IDs.")
  }

  # If `new_name` is not NULL, check that it is a single character string
  if (!is.null(new_name)) {
    if (!is.character(new_name) || length(new_name) != 1) {
      stop("'new_name' must be a single character string or NULL.")
    }
  }

  # 2. Identify the rows to keep based on model mod_id
  #    (the 'model_id' column in models$mod_gof)
  all_mod_id <- models$mod_gof[["model_id"]]
  keep_idx <- which(all_mod_id %in% mod_id)

  if (length(keep_idx) == 0) {
    stop("No model matched the requested mod_id. Check the 'mod_id' argument.")
  }

  # 3. Subset the relevant components
  new_mod_gof <- models$mod_gof[keep_idx, , drop = FALSE]
  new_mod_id <- new_mod_gof[["model_id"]]
  new_fitted <- models$fitted[new_mod_id]
  new_fixed <- models$fixed[new_mod_id]
  new_random <- models$random[new_mod_id]
  new_vcov <- models$vcov[new_mod_id]
  new_formulas <- models$formulas[new_mod_id]
  new_inlamod <- models$inlamod[new_mod_id]
  new_ppd <- models$ppd[new_mod_id]

  # 4. Reconstruct a new GHRmodels object
  out <- list(
    mod_gof = new_mod_gof,
    fitted = new_fitted,
    fixed = new_fixed,
    random = new_random,
    vcov = new_vcov,
    formulas = new_formulas,
    inlamod = new_inlamod,
    ppd = new_ppd,
    family = models$family,
    data = models$data,
    re = models$re,
    outcome = models$outcome
  )

  # 5. If 'new_name' are provided attach them
  if (!is.null(new_name)) {
    old_ids <- out$mod_gof$model_id

    if (length(new_name) == 1L) {
      # one prefix, all models share the same counter
      total_models <- length(old_ids)
      new_ids <- paste0(new_name, seq_len(total_models))
    } else {
      stop("'new_name' must be length 1")
    }

    # Replace IDs *everywhere*

    out$mod_gof$model_id <- new_ids
    names(out$formulas) <- new_ids
    names(out$fitted) <- new_ids
    names(out$fixed) <- new_ids
    names(out$random) <- new_ids
    names(out$vcov) <- new_ids
    names(out$inlamod) <- new_ids
    names(out$ppd) <- new_ids
  }

  #7. Return the object

  class(out) <- c("GHRmodels", class(out))
  return(out)
}
