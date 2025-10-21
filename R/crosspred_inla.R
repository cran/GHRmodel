#' Generate DLNM Predictions from `GHRmodels` Objects
#'
#' This function takes an object of class \code{GHRmodels}, extracts the relevant
#' coefficients and variance-covariance matrix, and then calls
#' \link[dlnm:crosspred]{dlnm::crosspred} to compute predictions over a range of covariate
#' values (or at specified points). 
#'
#' @param models An object of class \code{GHRmodels}, containing fitted model output
#'   (e.g., \code{$fixed} and \code{$vcov} lists).
#' @param basis A cross-basis or one-basis object, typically created by
#'  \code{\link{crossbasis_inla}} or \code{\link{onebasis_inla}}.
#' @param mod_id An integer or character string specifying which model within
#'   the input \code{GHRmodels} object to use (e.g., if \code{model$fixed} and \code{model$vcov} both
#'   have multiple entries).
#' @param at A numeric vector of values at which to compute predictions (e.g.,`seq(10,25, by=0.2)`)
#' @param from,to Numeric values specifying the range of the prediction sequence
#'   if \code{at} is not specified (e.g., `from = 10` and `to = 25`). 
#' @param by Numeric increment for the sequence if \code{at} is not specified
#'  (e.g., `by = 0.2`).
#' @param lag A vector of two elements with min and max lag as declared in the
#'  \code{crossbasis_inla} function.
#' @param bylag Numeric increment for lag steps (default is 1).
#' @param cen A centering value (e.g., a reference exposure level).
#' @param ci.level The credible interval level (default \code{0.95}).
#' @param cumul Logical; if \code{TRUE}, cumulative predictions are computed
#'   (default \code{FALSE}).
#' @param ... Additional arguments passed on to \link[dlnm]{crosspred},
#'   such as \code{bound}, \code{ci.arg}, etc.
#'
#' @details
#' The function identifies which coefficients in \code{model$fixed[mod_id]} and
#' which rows/columns in \code{model$vcov[mod_id]} correspond to the one-basis or 
#' cross-basis terms (i.e., matching the column names in \code{basis}). Then it passes these
#' slices to \link[dlnm:crosspred]{dlnm::crosspred} to generate predictions. The centering
#' value (\code{cen}), if specified, indicates the reference exposure (e.g., a mean
#' temperature) at which to center the effect estimates (e.g., the effect a given temperature value on the outcome 
#' will be compared to the effect of the centering value on the outcome,
#'  in this case the mean temperature).
#'
#' @return An object of class \code{"GHRcrosspred"}, inheriting from
#'   \code{"crosspred"}, with fields for the predicted values, credible intervals,
#'   and optionally cumulative predictions, as determined by
#'   \link[dlnm:crosspred]{crosspred}.
#'
#' @seealso \link[dlnm:crosspred]{dlnm::crosspred} for details on how predictions are computed.
#'
#' @importFrom dlnm crosspred
#' @export
#'
#' @examples
#' # Load example GHRmodels object from the package
#' model_dlnm_file <- system.file("examples", "model_dlnm.rds", package = "GHRmodel")
#' model_dlnm <- readRDS(model_dlnm_file)
#' 
#' # Load example cross-basis matrix from the package: 2-dimensional cross-basis matrix of the 
#' # non-linear effect of dengue risk across tmin values and lags: 
#' cb_tmin_file <- system.file("examples","cb_tmin.rds", package = "GHRmodel")
#' cb_tmin <- readRDS(cb_tmin_file) # loads cross-basis matrix into the environment
#'
#' # Generate predictions
#' pred_result <- crosspred_inla(
#'   models    = model_dlnm,
#'   basis    = cb_tmin,
#'   mod_id = "mod3",
#'   at       = seq(17, 24, by = 1),  # e.g., temperature sequence
#'   lag      = 2,
#'   cen      = 20,
#'   ci.level = 0.95
#' )
#'
#' # Inspect predictions
#' pred_result$predvar  # the sequence of 'at' values
#' pred_result$allfit   # fitted values
#' pred_result$alllow   # lower CI
#' pred_result$allhigh  # upper CI


crosspred_inla <- function(models,
                           basis,
                           mod_id,
                           at       = NULL,
                           from     = NULL,
                           to       = NULL,
                           by       = NULL,
                           lag,
                           bylag    = 1,
                           cen      = NULL,
                           ci.level = 0.95,
                           cumul    = FALSE,
                           ...) {
  
  # Argument checks
  if (!inherits(models, "GHRmodels")) {
    stop("'models' must be an object of class 'GHRmodels'.")
  }
  
  if (!is.matrix(basis) && !inherits(basis, "crossbasis") && !inherits(basis, "onebasis")) {
    stop("'basis' must be a cross-basis or one-basis object (typically from 'crossbasis_inla' or 'onebasis_inla').")
  }
  
  if (!mod_id %in% names(models$fixed)) {
    stop("The specified 'mod_id' is not found in 'models$fixed'.")
  }
  
  if (!mod_id %in% names(models$vcov)) {
    stop("The specified 'mod_id' is not found in 'models$vcov'.")
  }
  
  # Extract relevant coefficients and vcov for the specified model
  coef_df <- as.data.frame(models$fixed[mod_id])
  vcov_df <- models$vcov[[mod_id]]
  
  if (is.null(vcov_df)) {
    stop("Variance-covariance matrix (vcov) for 'mod_id' is NULL. Was the model fitted with config = TRUE and vcov = TRUE?")
  }
  
  # Match basis columns to coefficient names
  indt <- which(rownames(coef_df) %in% colnames(basis))
  
  if (length(indt) == 0) {
    stop("No matching coefficients found for provided basis. Make sure basis column names match model coefficient names.")
  }
  
  # Call dlnm::crosspred
  result <- dlnm::crosspred(
    basis    = basis,
    coef     = coef_df[, 1][indt],
    vcov     = as.matrix(vcov_df[indt, indt]),
    at       = at,
    from     = from,
    to       = to,
    by       = by,
    lag      = lag,
    bylag    = bylag,
    cen      = cen,
    ci.level = ci.level,
    cumul    = cumul,
    ...
  )
  
  class(result) <- append("GHRcrosspred", "crosspred")
  return(result)
}

