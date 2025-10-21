#' Generate INLA-compatible Model Formulas 
#' 
#'  This function streamlines the creation of INLA-compatible model formulas
#'  by automatically structuring fixed effects, random effects, and interactions.
#'  It accepts a list of covariate sets and produces a corresponding set of model formulas that 
#'  share a common random effect structure.
#'
#' @param outcome Character string specifying the name of the outcome variable.
#' @param covariates A list of character vectors, where each vector contains covariate names 
#'   to be included in the model. If a single vector is provided, a single model formula is generated.
#' @param baseline Logical; If `TRUE`, a baseline formula without covariates is included.
#' If no random effects are specified, this will be an intercept-only model. 
#' If random effects are specified, the baseline formula will include random effects
#' but not covariates. This formula will be the first in the list. Default is `TRUE`.
#' @param re1 A list defining a random effect structure. Up to five such lists (`re1` through `re5`) can be passed.
#' @param re2 Additional random effect definitions, as described for `re1`.
#' @param re3 Additional random effect definitions, as described for `re1`.
#' @param re4 Additional random effect definitions, as described for `re1`.
#' @param re5 Additional random effect definitions, as described for `re1`.
#'
#' @return A character vector of INLA model formulas.
#'
#' @details
#' The `write_inla_formulas()` function simplifies the creation of multiple INLA models 
#' by automatically structuring fixed effects, random effects, and interactions. The function 
#' ensures that all models have a consistent structure, making them easier to analyze and modify.
#'
#' If `baseline = TRUE`, a null formula (without covariates) is included as 
#' the first element of the list. 
#' 
#' The number of formulas generated depends on the length of the `covariates` list.
#' 
#' Random effects can be added using `re1, ..., re5`, where each effect must be a named list 
#' (e.g. re1 = list(id    = "year_id", model = "rw1")).
#' In the list the following fields are strictly necessary:
#'   - `id` *(character)*: the variable name that indexes the random effect (e.g., "year", "region").
#'   - `model` *(character)*: the type of random effect. Supported values include:
#'     `"iid"`, `"rw1"`, `"rw2"`, `"bym"`, and `"bym2"`.
#' - The following optional fields can be provided in the random effect list:
#'   - `replicate` *(character)*: defines an additional variable used to replicate the random effect structure across groups (e.g., spatial units for repeated time-series).
#'   - `group` *(character)*: used to model group-specific effects or nested structures.
#'   - `graph` *(character)*: required for `"bym"` and `"bym2"` models; refers to the name of an object in the environment that holds the spatial adjacency matrix.
#'   - `cyclic` *(logical)*: indicates whether the random walk (`"rw1"` or `"rw2"`) is cyclic. Default is `FALSE`. Use for periodic structures (e.g., months).
#'   - `scale.model` *(logical)*: if `TRUE`, scales structured random effects (like `rw1`, `rw2`, `bym`) so the generalized variance is 1. For `bym2` INLA automatically
#'   applies `scale.model = TRUE` internally.
#'   - `constr` *(logical)*: If `TRUE`, a sum to zero constrain is introduced. This 'constr' option is applied only to 'iid' random effects.  For `rw`, `ar`, `bym`, `bym2` INLA automatically
#'   applies `scale.model = TRUE` internally. 
#'   - `adjust.for.con.comp` *(logical)*: if `TRUE`, accounts for disconnected components in spatial graphs. Recommended for `"bym"` and `"bym2"`. Default is `FALSE`.
#'   - `hyper` *(character)*: the name of an object in the environment that contains the hyperprior specification for the random effect's precision or other parameters.
#'
#' For more information on random effects in R-INLA, see [Bayesian inference with INLA: Mixed-effects Models](https://becarioprecario.bitbucket.io/inla-gitbook/ch-mixed.html).
#'
#' @seealso \code{\link{as_GHRformulas}} for transforming model formulas into structured objects.
#'
#' @export
#' 
#' @examples
#' 
#' # Define covariates of interest
#' covs <- c("tmin.l1", "tmin.l2", "pdsi.l1", "pdsi.l2", "urban_level")
#'
#' # Combine covariate names using a pattern-matching functionality
#' combined_covariates <- cov_multi(
#'   covariates = covs,
#'   pattern    = c("tmin", "pdsi", "urban_level")
#' )
#'
#' # Define hyperprior specifications for random effects
#' prior_re1 <- list(prec = list(prior = "loggamma", param = c(0.01, 0.01)))
#' prior_re2 <- list(prec = list(prior = "loggamma", param = c(0.01, 0.01)))
#' prior_re3 <- list(
#'   prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01)),
#'   phi  = list(prior = "pc",      param = c(0.5, 2 / 3))
#' )
#'
#' # Write a set of INLA-compatible model formulas
#' inla_formulas <- write_inla_formulas(
#'   outcome    = "dengue_cases",
#'   covariates = combined_covariates,
#'   re1 = list(
#'     id        = "month_id",
#'     model        = "rw1",
#'     cyclic    = TRUE,
#'     hyper     = "prior_re1",
#'     replicate = "spat_meso_id"
#'   ),
#'   re2 = list(
#'     id    = "year_id",
#'     model    = "rw1",
#'     hyper = "prior_re2"
#'   ),
#'   re3 = list(
#'     id    = "spat_id",
#'     model    = "iid",
#'     hyper = "prior_re3"
#'   ),
#'   baseline = TRUE
#' )

write_inla_formulas <- function(outcome,
                                covariates = NULL,
                                baseline = TRUE,
                                re1 = list(id = NULL, 
                                           model = NULL, 
                                           replicate = NULL,
                                           group = NULL,
                                           graph = NULL,
                                           cyclic = FALSE,
                                           scale.model = FALSE, 
                                           constr = FALSE, 
                                           adjust.for.con.comp = FALSE, 
                                           hyper = NULL),
                                re2 = NULL, re3 = NULL, re4 = NULL, re5 = NULL) {
  
  # 1) Argument Checks ----
  
  if (missing(outcome)) stop("The 'outcome' argument is required but was not provided.")
  if (!is.character(outcome) || length(outcome) != 1) stop("'outcome' must be a single character string.")
  
  if (!is.null(covariates)) {
    if (!is.list(covariates)) stop("'covariates' must be a list of character vectors.")
    if (!all(sapply(covariates, is.character))) stop("All elements in 'covariates' must be character vectors.")
  }
  
  if (!is.logical(baseline) || length(baseline) != 1) {
    stop("'baseline' must be a single logical value (TRUE or FALSE).")
  }
  
  allowed_model <- c("iid", "rw1", "rw2", "bym", "bym2")
  re_list <- list(re1, re2, re3, re4, re5)
  
  # Validate and normalize random effect inputs
  for (i in seq_along(re_list)) {
    re <- re_list[[i]]
    if (!is.null(re) && (!is.null(re$id) || !is.null(re$model))) {
      if (!is.list(re)) stop("'re' must be a list if provided.")
      if (is.null(re$id) || is.null(re$model)) stop("'re' must include both 'id' and 'model'.")
      if (!is.character(re$id) || length(re$id) != 1) stop("'re$id' must be a single character string.")
      if (!is.character(re$model) || length(re$model) != 1 || !(re$model %in% allowed_model)) {
        stop(sprintf("'re$re' must be one of: %s", paste(allowed_model, collapse = ", ")))
      }
    }
  }
  
  # Replace NULL random effects with default structure
  default_re <- list(id = NULL, model = NULL, replicate = NULL, group = NULL, graph = NULL,
                     cyclic = FALSE, scale.model = TRUE,  constr = FALSE, adjust.for.con.comp = FALSE, hyper = NULL)
  re_list <- lapply(re_list, function(x) if (is.null(x)) default_re else x)
  
  # Keep only valid random effects (must have both id and re)
  re_list <- Filter(function(x) !is.null(x$id) && !is.null(x$model), re_list)
  
  # 2) Normalize covariate input ----
  if (is.null(covariates) || length(covariates) == 0 || all(sapply(covariates, length) == 0)) {
    covariates <- NULL
  }
  
  # 3) Generate fixed effects formulas ----
  fix.formulas <- character(0)
  if (!is.null(covariates)) {
    fix.formulas <- sapply(covariates, function(vars) {
      paste(outcome, "~ 1", if (length(vars) > 0) paste("+", paste(vars, collapse = " + ")) else "")
    })
  }
  
  # 4) Generate random effect terms ----
  re_terms <- list()
  if (length(re_list) > 0) {
    names(re_list) <- paste0("re_", seq_along(re_list))
    re_terms <- lapply(re_list, function(x) do.call(.write_re_inla, args = x))
    re_terms <- unlist(re_terms)
  }
  
  # 5) Add random effects to each formula ----
  if (length(fix.formulas) > 0 && length(re_terms) > 0) {
    fix.formulas <- paste(fix.formulas, "+", paste(re_terms, collapse = " + "))
  }
  
  # 6) Add baseline formula if requested ----
  formulas <- fix.formulas
  if (baseline) {
    base_formula <- paste(outcome, "~ 1", if (length(re_terms) > 0) paste("+", paste(re_terms, collapse = " + ")) else "")
    formulas <- c(base_formula, formulas)
  }
  
  # 7) Warn if no formulas created ----
  if (length(formulas) == 0) {
    warning("No formulas were generated: both 'covariates' and 'baseline' are empty or NULL.")
  }
  
  # 8) Return the formulas ----
  return(formulas)
}
