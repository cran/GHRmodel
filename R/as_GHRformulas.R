#' Convert R-INLA Model Formulas into a GHRformulas Object
#'
#' This function converts a character vector of suitable R-INLA formulas into a
#' structured `GHRformulas` object. The `GHRformulas` object contains the
#' standardized information about the fixed effects, the random effects, and the
#' outcome variable, ensuring consistency across multiple models to be fitted
#' using the \code{\link{fit_models}} function.
#'
#' @param formulas A character vector of model formulas formatted for R-INLA.
#' Each formula must contain a single `~` separating the outcome variable from
#' the predictors, and its right-hand side must be syntactically valid R code
#' (it is parsed internally to identify terms). Formulas generated with
#' \code{\link{write_inla_formulas}} are compatible with this function.
#'
#' @return A structured list of class `GHRformulas` with the components:
#' \describe{
#'   \item{`formulas`}{A character vector identical to the input `formulas`
#'                     (unmodified, in the same order).}
#'   \item{`vars`}{A data frame where each row corresponds to a formula and
#'                 each column to a covariate slot. Cells hold the covariate
#'                 term present at that position for the given formula, or
#'                 `NA` if that formula has fewer terms than the row with
#'                 the most.}
#'   \item{`re`}{A character vector of the random effects specified across
#'              all formulas (identical across formulas by construction, see
#'              Details). `character(0)` if none are present; otherwise
#'              named `"re_1"`, `"re_2"`, ... in the order they were found.}
#'   \item{`outcome`}{A character string indicating the outcome variable (must
#'                    be consistent across formulas).}
#' }
#'
#' @details
#' The `as_GHRformulas()` function parses each input formula to extract the
#' outcome variable, fixed effects (covariates), and random effects. The
#' resulting `GHRformulas` object is designed to be used with the
#' \code{\link{fit_models}} function for model fitting with R-INLA.
#'
#' @seealso \code{\link{write_inla_formulas}} to generate R-INLA compatible
#' input formulas.
#'
#' @export
#'
#' @examples
#' # Define formulas
#' formulas <- c(
#' "dengue_cases ~ 1 + f(month_id, model = 'rw1')",
#' "dengue_cases ~ 1 + f(month_id, model = 'rw1') + tmin.l1")
#'
#' # Convert the formulas into a GHRformulas object
#' formulas <- as_GHRformulas(formulas)
#'
#' # Inspect the structured GHRformulas object
#' print(formulas)
#' # Visualize output: GHRformulas object
#' class(formulas)

as_GHRformulas <- function(formulas) {
  # 1) Checks ----
  if (length(formulas) < 1) {
    stop("No formulas provided.")
  }

  if (!is.character(formulas)) {
    stop("'formulas' must be a character vector.")
  }

  if (any(duplicated(formulas))) {
    warning("Duplicate formulas detected.")
  }

  # 2) Prepare storage ----
  all_outcomes <- character(length(formulas))
  # Fixed effects + Spatially Varying Covariates (SVC)
  all_covariates <- vector("list", length(formulas))
  all_random_effects <- vector("list", length(formulas)) # Random effects

  # 3) Main loop over each formula ----
  for (i in seq_along(formulas)) {
    fstr <- formulas[[i]]

    # Split at '~'
    parts <- strsplit(fstr, "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      stop("Each formula must contain exactly one '~':\n", fstr)
    }

    lhs <- trimws(parts[1]) # Left Hand Side (Outcome)
    rhs <- trimws(parts[2]) # Right Hand Side (Predictors and Random effects)

    # Outcomes
    all_outcomes[i] <- lhs

    # Split RHS on '+' on the top level of the formula
    # (not in + within parentheses or quotes)
    terms_raw <- trimws(.split_top_level(rhs, "+"))

    # Classify each chunk (Covariate, Spatially Varying Coef, or Random Effect)
    cov_i <- character()
    re_i <- character()

    for (term in terms_raw) {
      # Strip an explicit intercept removal so it doesn't get glued onto
      # the preceding term as literal
      minus_parts <- .split_top_level(term, "-")
      if (length(minus_parts) == 2 && trimws(minus_parts[2]) == "1") {
        term <- trimws(minus_parts[1])
      }

      # Skip empty or intercept
      if (term == "" || term == "1") {
        next
      }

      # Check for `f(...)` terms
      if (grepl("^f\\s*\\(", term)) {
        # Classify by argument structure, not by textual position, so that
        # Errors (rather than silently dropping the term) if the
        # term cannot be classified as a random effect, SVC, or non-linear
        cls <- .classify_f_term(term)
        if (cls$type == "svc") {
          cov_i <- c(cov_i, cls$value) # Store entire term as a covariate (SVC)
        } else if (cls$type == "re") {
          re_i <- c(re_i, cls$value) # Store as random effect
        } else if (cls$type == "nl") {
          cov_i <- c(cov_i, cls$value) # Store extracted covariate name
        }
      } else {
        # Normal covariate or potential interaction
        # Check if there's a '*' => expand
        if (grepl("\\*", term)) {
          expanded <- .expand_interaction(term)
          cov_i <- c(cov_i, expanded)
        } else {
          # If no '*', just keep as a single covariate
          cov_i <- c(cov_i, term)
        }
      }
    }

    all_covariates[[i]] <- cov_i
    all_random_effects[[i]] <- re_i
  }

  # 4) Build covariate data.frame ----
  covs_df <- .pad_to_df(all_covariates, "covariate_")

  # 5) Build random effects data.frame ----
  re_df <- .pad_to_df(all_random_effects, "re_")

  # 6) Build final object ----
  out <- list(
    formulas = formulas, # Original formulas
    vars = covs_df, # Data frame of covariates & SVCs
    re = re_df, # Data frame of random effects
    outcome = all_outcomes # Outcome variable
  )

  # Verify the outcome is the same across all formulas
  unique_out <- unique(out[["outcome"]])
  if (length(unique_out) != 1) {
    stop(
      "Error: different outcomes identified across formulas:\n",
      paste(unique_out, collapse = ", ")
    )
  } else {
    out[["outcome"]] <- unique_out[1]
  }

  # Verify that each random-effect column is identical across formulas, then
  # collapse to a single named character vector
  if (ncol(out$re) > 0) {
    for (jj in seq_len(ncol(out$re))) {
      unique_re_col <- unique(out$re[, jj])
      if (length(unique_re_col) != 1) {
        stop(
          "Error: different random effects ",
          "identified across formulas in column ",
          jj,
          " => ",
          paste(unique_re_col, collapse = ", ")
        )
      }
    }
    out$re <- vapply(out$re, function(col) as.character(col[1]), character(1))
  } else {
    out$re <- character(0)
  }

  # 7) Return the output ----
  class(out) <- "GHRformulas"
  return(out)
}
