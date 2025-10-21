#' Convert R-INLA Model Formulas into a GHRformulas Object
#'
#' This function converts a character vector of suitable R-INLA formulas into a structured `GHRformulas` object.
#' The `GHRformulas` object contains the standardized information about the fixed effects, the random effects, and the outcome variable,
#' ensuring consistency across multiple models to be fitted using the \code{\link{fit_models}} function.
#'
#' @param formulas A character vector of model formulas formatted for R-INLA. Each formula must contain
#'   a single `~` separating the outcome variable from the predictors. Formulas generated with
#'   \code{\link{write_inla_formulas}} are compatible with this function.
#'
#' @return A structured list of class `GHRformulas` with the following components:
#' \describe{
#'   \item{`formulas`}{A character vector of the original INLA-compatible model formulas.}
#'   \item{`vars`}{A data frame where each row corresponds to a formula and each column to a covariate.
#'                 Entries indicate whether a covariate is included in the formula.}
#'   \item{`re`}{A character vector listing the random effects specified across all formulas.}
#'   \item{`outcome`}{A character string indicating the outcome variable (must be consistent across formulas).}
#' }
#'
#' @details
#' The `as_GHRformulas()` function parses each input formula to extract the outcome variable,
#' fixed effects (covariates), and random effects. The resulting `GHRformulas` object is designed to be used
#' with the \code{\link{fit_models}} function for model fitting with R-INLA. 
#' 
#' @seealso \code{\link{write_inla_formulas}} to generate R-INLA compatible input formulas
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
    stop("No formulas provided.")}
  
  if (!is.character(formulas)) {
    stop("'formulas' must be a character vector.")
  }
  
  if (any(duplicated(formulas))) {
    warning("Duplicate formulas detected.")
  }
  
  
  # 2) Prepare storage ----
  all_outcomes       <- character(length(formulas))
  all_covariates     <- vector("list", length(formulas))  # Fixed effects + Spatially Varying Covariates (SVC)
  all_random_effects <- vector("list", length(formulas))  # Random effects
  
  # 3) Main loop over each formula ----
  for (i in seq_along(formulas)) {
    fstr <- formulas[[i]]
    
    # Split at '~'
    parts <- strsplit(fstr, "~", fixed = TRUE)[[1]]
    if (length(parts) != 2) stop("Each formula must contain exactly one '~':\n", fstr)
    
    lhs <- trimws(parts[1])  # Left Hand Side (Outcome)
    rhs <- trimws(parts[2])  # Right Hand Side (Predictors and Random effects)
    
    # Outcomes
    all_outcomes[i] <- lhs
    
    # Split RHS on '+'
    terms_raw <- trimws(strsplit(rhs, "\\+")[[1]])
    
    # Classify each chunk (Covariate, Spatially Varying Coef, or Random Effect)
    cov_i <- character()
    re_i  <- character()
    
    for (term in terms_raw) {
      # Skip empty or intercept
      if (term == "" || term == "1") next
      
      # Check for `f(...)` terms
      if (grepl("^f\\s*\\(", term)) {
        # Detect Spatially Varying Coefficients (SVCs) -> `f(id, cov, model = ...)`
        if (grepl("^f\\([^,]+,\\s*[^=]+,", term)) {
          cov_i <- c(cov_i, term)  # Store entire term as a covariate (SVC)
        } 
        # Detect standard random effects -> `f(id, model = ...)`
        else if (grepl("^f\\([^,]+,\\s*model\\s*=", term)) {
          re_i <- c(re_i, term)  # Store as random effect
        } 
        # If `inla.group(...)` is inside, process it and store as a covariate
        else if (grepl("inla\\.group", term)) {
          parsed <- .split_inla_group(term)
          cov_i <- c(cov_i, parsed)  # Store extracted covariate name
        }
      } else {
        # Normal covariate or potential interaction
        # Check if there's a '*' => expand
        if (grepl("\\*", term)) {
          expanded <- .expand_interaction(term)
          cov_i    <- c(cov_i, expanded)
        } else {
          # If no '*', just keep as a single covariate
          cov_i <- c(cov_i, term)
        }
      }
    }
    
    all_covariates[[i]]     <- cov_i
    all_random_effects[[i]] <- re_i
  }
  
  # 4) Build covariate data.frame ----
  max_covs <- max(lengths(all_covariates))
  if (max_covs == 0) {
    covs_df <- data.frame(matrix(nrow = length(formulas), ncol = 0))
  } else {
    covs_matrix <- do.call(rbind, lapply(all_covariates, function(cv) {
      length(cv) <- max_covs  # Fill with NA if shorter
      cv
    }))
    colnames(covs_matrix) <- paste0("covariate_", seq_len(max_covs))
    covs_df <- as.data.frame(covs_matrix, stringsAsFactors = FALSE)
  }
  
  # 5) Build random effects data.frame ----
  max_res <- max(lengths(all_random_effects))
  if (max_res == 0) {
    re_df <- data.frame(matrix(nrow = length(formulas), ncol = 0))
  } else {
    re_matrix <- do.call(rbind, lapply(all_random_effects, function(re) {
      length(re) <- max_res  # Pad with NA if shorter
      re
    }))
    colnames(re_matrix) <- paste0("re_", seq_len(max_res))
    re_df <- as.data.frame(re_matrix, stringsAsFactors = FALSE)
  }
  
  # 6) Build final object ----
  out <- list(
    formulas = formulas,    # Original formulas
    vars     = covs_df,     # Data frame of covariates & SVCs
    re       = re_df,       # Data frame of random effects
    outcome  = all_outcomes # Outcome variable
  )
  
  # Verify the outcome is the same across all formulas
  unique_out <- unique(out[["outcome"]])
  if (length(unique_out) != 1) {
    stop("Error: different outcomes identified across formulas:\n",
         paste(unique_out, collapse = ", "))
  } else {
    out[["outcome"]] <- unique_out[1]
  }
  
  # Verify that each random-effect column is identical across formulas
  if (ncol(out$re) > 0) {
    for (jj in seq_len(ncol(out$re))) {
      unique_re_col <- unique(out$re[, jj])
      if (length(unique_re_col) != 1) {
        stop("Error: different random effects identified across formulas in column ",
             jj, " => ", paste(unique_re_col, collapse = ", "))
      }
    }
    # If identical, collapse it to a single row 
    out$re <- unlist(out$re[1, ])
  }
  
  # 7) Return the output ----
  class(out) <- c("GHRformulas", "list")
  return(out)
}