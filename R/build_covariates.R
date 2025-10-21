#' Extract Covariate Names
#'
#' This function allows the user to select variables from a data set by prefix 
#' (using the `pattern` argument) or by exact name matching.
#' The return object is a character vector with the selected covariate names that can be used
#'  as input for \code{\link{cov_add}}, \code{\link{cov_uni}},
#' \code{\link{cov_multi}}, \code{\link{cov_interact}}, \code{\link{cov_nl}},
#'  and \code{\link{cov_varying}} functions. 
#' 
#' @param data A `data.frame` containing the variables.
#' @param pattern A character vector specifying prefix(es) to match (e.g., "tmin" matches "tmin", "tmin.l1", etc.).
#' @param name A character vector of exact variable name(s) to extract.
#'
#' @return A character vector of matched covariate names.
#' @export
#'
#' @examples
#' data <- data.frame(tmin = 1:10, tmin.l1 = 1:10, urban = 1:10)
#' extract_names(data, pattern = "tmin")
#' extract_names(data, name = "urban")
#' extract_names(data, pattern = "tmin", name = "urban")

extract_names <- function(data = NULL, pattern = NULL, name = NULL) {
  
  # 1) Checks ----
  if (is.null(data)) {
    stop("The 'data' argument must be provided.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (is.null(pattern) && is.null(name)) {
    stop("Either 'pattern' or 'name' argument must be provided.")
  }
  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character vector.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("'name' must be a character vector.")
  }
  
  # 2) Extract variables based on pattern or exact_name ----
  vars <- c()
  
  # Store patterns or names that don't exist in the data 
  missing_patterns <- c()
  missing_names <- c()
  
  # Pattern
  if (!is.null(pattern)) {
    for (i in seq_along(pattern)) {
      matching_cols <- names(dplyr::select(data, dplyr::starts_with(pattern[[i]], ignore.case = TRUE)))
      if (length(matching_cols) > 0) {
        vars <- c(vars, matching_cols)
      } else {
        missing_patterns <- c(missing_patterns, pattern[[i]])
      }
    }
  }
  
  # Exact Name 
  if (!is.null(name)) {
    for (n in name) {
      if (n %in% names(data)) {
        vars <- c(vars, n)
      } else {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  # 3) Ensure unique variables ----
  vars <- unique(vars)
  
  if (length(vars) == 0) {
    stop("No matching variables found in the dataset.")
  }
  
  # Error if any patterns/names missing
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  # 4) Return Output ----
  return(vars)
}

#' Build Univariable Covariate Sets
#'
#' This function returns a list where each element contains a single covariate, based on 
#' covariates specified in the `pattern` or `name` arguments. This structure is 
#' suitable for generating separate univariable model formulas using \code{\link{write_inla_formulas}}.
#'
#' @param covariates A character vector of covariate names. Typically the output from \code{\link{extract_names}}.
#' @param pattern A character vector specifying the prefix pattern(s) to match (e.g., "tmin" matches "tmin", "tmin.l1", etc.).
#' @param name A character vector specifying exact variable name(s) to extract.
#'
#' @return A list of character vectors, each of length 1, containing the matched covariate name.
#' The resulting list is suitable for use as the `covariates` argument in \code{\link{write_inla_formulas}}.
#'
#' @export
#'
#' @examples
#' data <- data.frame(tmin = rnorm(10), tmin.l1 = rnorm(10), urban = rnorm(10))
#' covs <- extract_names(data, pattern = "tmin", name = "urban")
#' cov_uni(covs, pattern = "tmin")
#' cov_uni(covs, name = "urban")

cov_uni <- function(covariates = NULL, pattern = NULL, name = NULL) {
  
  # 1) Checks ----
  if (is.null(pattern) && is.null(name)) {
    stop("At least one of 'pattern' or 'name' must be provided.")
  }
  if (!is.character(covariates)) {
    stop("'covariates' must be a character vector.")
  }
  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character vector.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("'name' must be a character vector.")
  }
  
  # 2) Store selected covariates as a list of single variables ----
  vars <- list()
  
  # Store patterns or names that don't exist in the data 
  missing_patterns <- c()
  missing_names <- c()
  
  # Pattern-based match
  if (!is.null(pattern)) {
    for (i in seq_along(pattern)) {
      matching_cols <- covariates[startsWith(covariates, prefix = pattern[[i]])]
      if (length(matching_cols) > 0) {
        vars <- c(vars, matching_cols)
      }else {
        missing_patterns <- c(missing_patterns, pattern[[i]])
      }
    }
  }
  
  # Exact match
  #if (!is.null(name)) {
  #  exact_matches <- covariates[covariates %in% name]
  # if (length(exact_matches) > 0) {
   #   vars <- c(vars, exact_matches)
   # }
  #}
  
  # Exact match
  if (!is.null(name)) {
    for (n in name) {
      if (n %in% covariates) {
        vars <- c(vars, n)
      } else {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  # Throw error if any missing patterns or names
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  # 3) Ensure unique + format as list of single variables ----
  vars <- as.list(unique(vars))
  
  if (length(vars) == 0) {
    stop("No matching variables found in the dataset.")
  }
  
  # 4) Return ----
  return(vars)
}


#' Create Covariate Combinations Across Groups
#'
#' This function generates all possible combinations of covariates by selecting one
#' variable from each user-defined group. Groups can be defined either by a regular expression pattern 
#' (`pattern`) or by exact variable names (`name`). The resulting list 
#' can be input into the `covariates` argument in \code{\link{write_inla_formulas}} to 
#' generate multivariable model formulas where all combinations of covariates are needed.
#' 
#' @param covariates A character vector or a list of single-element character vectors. Typically 
#' obtained from \code{\link{extract_names}} or \code{\link{cov_uni}} or \code{\link{cov_nl}}.
#' @param pattern A character vector of regular expression patterns  (e.g., "tmin" matches "tmin", "tmin.l1", etc.).
#'  Each pattern defines a group to draw covariates from.
#' @param name A character vector of exact variable names to include as an additional group.
#' @param add Logical; if `TRUE`, appends the generated combinations to the original `covariates` object. Default is `FALSE`.
#'
#' @return A list of character vectors. Each element is a unique combination of covariates,
#'         where one variable is drawn from each specified group. The resulting list is 
#'         suitable as input in the `covariates` argument in \code{\link{write_inla_formulas}}.
#'
#' @export
#'
#' @examples
#' data <- data.frame(tmin = rnorm(10), tmin.l1 = rnorm(10),
#'                    pdsi = rnorm(10), urban = rnorm(10))
#' 
#' # Extract covariate names
#' covs <- extract_names(data, pattern = c("tmin", "pdsi", "urban"))
#'
#' # Combine "tmin" and "pdsi" into all possible pairings
#' cov_multi(covariates = covs, pattern = c("tmin", "pdsi"))
#'
#' # Combine "tmin" and "urban", treating "urban" as an exact match
#' cov_multi(covariates = covs, pattern = "tmin", name = "urban")
#'
#' # Use output as input to write_inla_formulas()
#' combined_covs <- cov_multi(covariates = covs, pattern = c("tmin", "pdsi"))
#' formulas <- write_inla_formulas(outcome = "cases", covariates = combined_covs)

cov_multi <- function(covariates,
                      pattern = NULL,
                      name = NULL,
                      add = FALSE) {
  
  # 1) Argument Checks ----
  if (is.null(pattern) && is.null(name)) {
    stop("At least one of 'pattern' or 'name' must be provided.")
  }
  if (is.null(covariates)) stop("'covariates' must be provided.")
  
  # Allow list input of single-element vectors
  if (is.list(covariates)) {
    if (!all(sapply(covariates, function(x) is.character(x) && length(x) == 1))) {
      stop("If 'covariates' is a list, all elements must be single-element character vectors.")
    }
    covariates <- unlist(covariates, use.names = FALSE)
  }
  
  if (!is.character(covariates)) stop("'covariates' must be a character vector.")
  if (!is.null(name) && !is.character(name)) stop("'name' must be a character vector.")
  if (!is.logical(add) || length(add) != 1) stop("'add' must be TRUE or FALSE.")
  
  # 2) Collect matching groups ----
  groups <- list()
  missing_patterns <- c()
  missing_names <- c()
  
  # Pattern groups
  if (!is.null(pattern)) {
    for (pat in pattern) {
      matched <- grep(pat, covariates, value = TRUE)
      if (length(matched) > 0) {
        groups[[length(groups) + 1]] <- matched
      } else {
        missing_patterns <- c(missing_patterns, pat)
      }
    }
  }
  
  # Exact name group
  if (!is.null(name)) {
    for (n in name) {
      if (n %in% covariates) {
        groups[[length(groups) + 1]] <- n
      } else {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  # Throw error if any pattern/name didn’t match
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  if (length(groups) == 0) {
    stop("No matching covariates found.")
  }
  
  # 3) Create combinations ----
  if (length(groups) == 1) {
    result <- list(groups[[1]])
  } else {
    grid <- expand.grid(groups, stringsAsFactors = FALSE)
    result <- apply(grid, 1, function(x) as.character(x))
    result <- lapply(seq_len(ncol(result)), function(i) result[, i])
  }
  
  # Remove any list names (like $`1`, $`2`, etc.)
  names(result) <- NULL
  
  # 4) Optionally add full set ----
  if (add) {
    result <- c(result, list(unique(covariates)))
  }
  
  return(result)
}



#' Generate Interaction Terms Between Covariates
#'
#' This function generates interaction terms between covariates specified in the `pattern` or `name` arguments.
#' It requires a list of character vectors and appends interaction terms to each vector
#' based on pairwise or three-way interactions. The resulting list 
#' can be input into the `covariates` argument in \code{\link{write_inla_formulas}}.
#'
#' @param covariates A list of character vectors, each vector containing variable names.
#' Typically an output of \code{\link{cov_multi}} or \code{\link{cov_uni}}.
#' @param pattern A character vector of length 2 or 3 specifying prefixes of variables to interact 
#' (e.g., "tmin" matches "tmin", "tmin.l1", etc.).
#' @param name A character vector specifying the exact variable names to be included in the interactions.
#' @param add Logical; if `TRUE`, appends the newly created formulas to the original list. Default is `FALSE`.
#'
#' @return A list of character vectors, where each vector includes covariates and their corresponding 
#' interaction terms. This object can be passed to the `covariates` argument in \code{\link{write_inla_formulas}}.
#'
#' @details
#' - If two variables are matched, their pairwise interaction is added (`var1:var2`).
#' - If three variables are matched, two-way and three-way interactions are generated.
#' - Only variables that are expressed as linear terms can be used in interactions.
#' - Use either `pattern`, `name`, or both to identify variables for interaction.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(tmin.l1 = rnorm(10), pdsi.l1 = rnorm(10), urban = rnorm(10))
#'
#' # Extract names
#' covs <- extract_names(data, pattern = c("tmin", "pdsi", "urban"))
#'
#' # Create combinations
#' combos <- cov_multi(covariates = covs, pattern = c("tmin", "pdsi"))
#'
#' # Add interaction terms
#' cov_interact(covariates = combos, pattern = c("tmin", "pdsi"))
#'
#' # Output can be passed to write_inla_formulas()
#' new_covs <- cov_interact(combos, pattern = c("tmin", "pdsi"))
#' formulas <- write_inla_formulas(outcome = "cases", covariates = new_covs)

cov_interact <- function(covariates = NULL,
                         pattern = NULL,
                         name = NULL,
                         add = FALSE) {
  
  # 1) Checks ----
  if (!is.list(covariates)) {
    stop("'covariates' must be a list of character vectors.")
  }
  if (!all(sapply(covariates, is.character))) {
    stop("Each element of 'covariates' must be a character vector.")
  }
  if (is.null(pattern) && is.null(name)) {
    stop("At least one of 'pattern' or 'name' must be provided.")
  }
  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character vector if provided.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("'name' must be a character vector if provided.")
  }
  if (!is.logical(add) || length(add) != 1) {
    stop("'add' must be a single logical value (TRUE or FALSE).")
  }
  
  
  # Check for invalid patterns or names ----
  all_covs <- unique(unlist(covariates))
  missing_patterns <- c()
  missing_names <- c()
  
  # Non-linear covariates are not accepted 
  nonlinear <- grep("^f\\(INLA::inla.group\\(", all_covs, value = TRUE)
  if (length(nonlinear) > 0) {
    stop(
      paste0(
        "The following are non-linear and not supported: ",
        paste(nonlinear, collapse = ", ")
      )
    )
  }
  
  # Check for invalid patterns 
  if (!is.null(pattern)) {
    for (p in pattern) {
      if (!any(startsWith(all_covs, p))) {
        missing_patterns <- c(missing_patterns, p)
      }
    }
  }
  
  # Check for invalid names
  if (!is.null(name)) {
    for (n in name) {
      if (!(n %in% all_covs)) {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  # 2) Identify interaction variables per set ----
  interaction_terms_list <- NULL
  
  for (i in seq_along(covariates)) {
    covs <- as.character(unlist(covariates[i]))
    matched_names <- character()
    
    for (var in covs) {
      is_match <- (!is.null(pattern) && any(startsWith(var, pattern))) ||
        (!is.null(name) && var %in% name)
      if (is_match) matched_names <- c(matched_names, var)
    }
    
    if (length(matched_names) %in% c(2, 3)) {
      interaction_terms_list[[i]] <- .build_interaction(matched_names)
    } else {
      interaction_terms_list[[i]] <- NA
    }
  }
  
  # 3) Append interaction terms ----
  new_covariates <- mapply(c, covariates, interaction_terms_list, SIMPLIFY = FALSE)
  new_covariates <- lapply(new_covariates, function(x) as.vector(stats::na.omit(unlist(x))))
  
  # 4) Optionally include original list ----
  if (isTRUE(add)) {
    new_covariates <- c(covariates, new_covariates)
    new_covariates <- unique(new_covariates)
  }
  
  # 5) Return result ----
  return(new_covariates)
}


#' Create Non-Linear Effects for INLA
#'
#' This function transforms selected covariates identified by `pattern` or `name` into non-linear 
#' terms using INLA's `f()` syntax. It supports random walk models (`rw1`, `rw2`) and allows discretization 
#' by quantiles or equal intervals. Transformed covariates are returned as character vectors inside a list
#' ready to be passed to the \code{\link{write_inla_formulas}} function.
#'
#' @param covariates A character vector or list of character vectors. Usually from 
#' \code{\link{cov_multi}} or \code{\link{cov_uni}}.
#' @param pattern Character vector of patterns to match covariates for transformation
#' (e.g., "tmin" matches "tmin", "tmin.l1", etc.).
#' @param name Character vector of exact covariate names to transform.
#' @param model Character; either `"rw1"` or `"rw2"` to specify the non-linear INLA model.
#' @param method Character; either `"cut"` or `"quantile"` for discretization. Default is `"quantile"`.
#' @param n Integer; number of intervals or quantile bins. Must be >= 2. Default is 10.
#' @param replicate Optional character string indicating a replicate structure for non-linear effects.
#' @param add Logical; if `TRUE`, adds the transformed covariates to the original ones. Default is `FALSE`.
#'
#' @return A list of character vectors. This object can be passed to the `covariates` 
#' argument in \code{\link{write_inla_formulas}}.
#'
#' @details
#' - Use `pattern` or `name` (or both) to specify which variables to transform.
#' - The method and n arguments discretize the covariate into evenly populated bins.
#' - The function supports discretization with either equal-width (`cut`) or quantile-based (`quantile`) bins.
#' - The model argument imposes smoothness on the grouped effect, capturing non-linear trends.
#' - Non-linear effects are created using `.single_non_linear_eff_inla()` (internal helper).
#'
#' @seealso See \href{https://becarioprecario.bitbucket.io/inla-gitbook/ch-smoothing.html}{Bayesian inference with INLA: Smoothing} 
#'  for more information on smoothing and non-linear effects in R-INLA models.
#'
#' @export
#'
#' @examples
#' data <- data.frame(tmin.l1 = rnorm(10), pdsi.l1 = rnorm(10))
#'
#' covs <- extract_names(data, pattern = c("tmin", "pdsi"))
#' covlist <- cov_multi(covs, pattern = c("tmin", "pdsi"))
#'
#' # Apply non-linear transformation to tmin variables
#' cov_nl(covlist, pattern = "tmin", model = "rw2")
#'
#' # Include original variables along with transformed ones
#' cov_nl(covlist, pattern = "tmin", model = "rw2", add = TRUE)

cov_nl <- function(covariates,
                   pattern = NULL,
                   name = NULL,
                   model = "rw2",
                   method = "quantile",
                   n = 10,
                   replicate = NULL,
                   add = FALSE) {
  
  # 1) Checks ----
  if (missing(covariates)) stop("'covariates' must be provided.")
  if (!is.character(covariates) && !is.list(covariates)) {
    stop("'covariates' must be a character vector or list of character vectors.")
  }
  if (is.list(covariates) && !all(sapply(covariates, is.character))) {
    stop("If 'covariates' is a list, all elements must be character vectors.")
  }
  if (is.null(pattern) && is.null(name)) {
    stop("At least one of 'pattern' or 'name' must be provided.")
  }
  if (!model %in% c("rw1", "rw2")) {
    stop("'model' must be either 'rw1' or 'rw2'.")
  }
  if (!method %in% c("cut", "quantile")) {
    stop("'method' must be either 'cut' or 'quantile'.")
  }
  if (!is.numeric(n) || n != floor(n) || n < 2) {
    stop("'n' must be an integer >= 2.")
  }
  if (!is.null(replicate) && !is.character(replicate)) {
    stop("'replicate' must be a character string or NULL.")
  }
  if (!is.logical(add) || length(add) != 1) {
    stop("'add' must be a single logical value.")
  }
  
  # 2) Normalize to list of vectors ----
  cov_list <- if (is.character(covariates)) as.list(covariates) else covariates
  
  # 3) Check if all requested patterns/names exist ----
  all_covs <- unique(unlist(cov_list))
  
  missing_patterns <- c()
  if (!is.null(pattern)) {
    for (p in pattern) {
      if (!any(startsWith(all_covs, p))) {
        missing_patterns <- c(missing_patterns, p)
      }
    }
  }
  
  missing_names <- c()
  if (!is.null(name)) {
    for (n in name) {
      if (!(n %in% all_covs)) {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  # 4) Identify and transform matching variables ----
  should_transform <- function(var) {
    matches_pattern <- if (!is.null(pattern)) any(sapply(pattern, function(p) grepl(p, var))) else FALSE
    matches_name <- if (!is.null(name)) var %in% name else FALSE
    matches_pattern || matches_name
  }
  
  transform_set <- function(vec) {
    transformed <- vec
    for (i in seq_along(vec)) {
      if (should_transform(vec[i])) {
        transformed[i] <- .single_non_linear_eff_inla(
          covariate = vec[i],
          pattern = pattern,
          name = name,
          model = model,
          method = method,
          n = n,
          replicate = replicate
        )
      }
    }
    # Return list
    # 1st element $original is the orginal vector input in the covariates argument
    # 2nd element $ transformed is a vector where the selected variables are transformed
    list(original = vec, transformed = transformed)
  }
  # Apply transformation logic to each vector in the list
  result_pairs <- lapply(cov_list, transform_set)
  # Separate out original and transformed variable vectors
  originals <- lapply(result_pairs, `[[`, "original")
  transformed <- lapply(result_pairs, `[[`, "transformed")
  
  # 5) Combine output ----
  if (add) {
    all_sets <- c(originals, transformed)
    keys <- unique(lapply(all_sets, function(x) paste(sort(x), collapse = "|")))
    result <- lapply(keys, function(k) unlist(strsplit(k, "\\|")))
  } else {
    result <- transformed
  }
  
  return(result)
}


#' Create Spatially or Temporally Varying Effects for INLA
#'
#' This function transforms covariates identified by `pattern` or  `name` into
#' varying effect terms of the form:\code{f(unit, covariate, model = 'iid')},
#' which allows covariates to have varying slopes across spatial or temporal units. 
#' The output can be used directly in the `covariates` argument of \code{\link{write_inla_formulas}}.
#'
#' @param covariates A character vector or a list of character vectors of covariate names. 
#' Typically output from \code{\link{cov_multi}}, \code{\link{cov_uni}}, or \code{\link{extract_names}}.
#' @param unit Character string specifying the unit of variation (e.g., `"spat_id"`, `"year"`).
#' @param pattern A character vector specifying the prefix pattern(s) to match (e.g., "tmin" matches "tmin", "tmin.l1", etc.)
#'  for transformation.
#' @param name Character vector of exact variable names to be transformed.
#' @param model Character string specifying the INLA model for the varying effect. Currently, only `"iid"` is supported.
#' @param constr Logical. If TRUE it will impose a sum-to-zero constraint to the random effect.
#' Default is FALSE.
#' @param add Logical; if `TRUE`, appends the transformed covariates to the original ones. Default is `FALSE`.
#' 
#' @return A list of character vectors, each including covariates with varying effects.
#' The output is suitable as input for \code{\link{write_inla_formulas}}.
#'
#' @details
#' - Use `pattern` or `name` (or both) to specify which covariates to transform.
#' - The resulting terms use INLA’s `f()` syntax: \code{f(unit, covariate, model = "iid")}.
#' - Currently only supports `"iid"` models for varying effects.
#'
#' @export
#'
#' @examples
#' data <- data.frame(tmin.l1 = rnorm(10), pdsi.l1 = rnorm(10))
#'
#' covs <- extract_names(data, pattern = c("tmin", "pdsi"))
#' covlist <- cov_multi(covs, pattern = c("tmin", "pdsi"))
#'
#' # Apply varying effect to tmin
#' cov_varying(covlist, pattern = "tmin", unit = "spat_id")
#'
#' # Keep original and add varying effect terms
#' cov_varying(covlist, pattern = "tmin", unit = "spat_id", add = TRUE)

cov_varying <- function(covariates,
                        unit,
                        pattern = NULL,
                        name = NULL,
                        model = 'iid',
                        constr = FALSE,
                        add = FALSE) {
  
  # 1) Checks ----
  if (missing(covariates)) stop("'covariates' must be provided.")
  if (!is.character(covariates) && !is.list(covariates)) {
    stop("'covariates' must be a character vector or a list of character vectors.")
  }
  if (is.list(covariates) && !all(sapply(covariates, is.character))) {
    stop("If 'covariates' is a list, all elements must be character vectors.")
  }
  if (missing(unit) || !is.character(unit) || length(unit) != 1) {
    stop("'unit' must be a single character string.")
  }
  if (!is.character(model) || length(model) != 1 || !(model %in% "iid")) {
    stop("'model' must be 'iid'. Other models are not yet supported.")
  }
  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character vector if provided.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("'name' must be a character vector if provided.")
  }
  if (!is.logical(add) || length(add) != 1) {
    stop("'add' must be a single logical value.")
  }
  
  # 2) Normalize to list of vectors ----
  cov_list <- if (is.character(covariates)) list(covariates) else covariates
  
  # 3) Check for missing patterns/names ----
  all_covs <- unique(unlist(cov_list))
  missing_patterns <- c()
  missing_names <- c()
  
  if (!is.null(pattern)) {
    for (p in pattern) {
      if (!any(startsWith(all_covs, p))) {
        missing_patterns <- c(missing_patterns, p)
      }
    }
  }
  
  if (!is.null(name)) {
    for (n in name) {
      if (!(n %in% all_covs)) {
        missing_names <- c(missing_names, n)
      }
    }
  }
  
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  
  # 4) Identify matching covariates ----
  should_transform <- function(var) {
    matches_pattern <- if (!is.null(pattern)) any(sapply(pattern, function(p) grepl(p, var))) else FALSE
    matches_name <- if (!is.null(name)) var %in% name else FALSE
    matches_pattern || matches_name
  }
  
  # 5) Transform matching variables ----
  transform_set <- function(vec) {
    transformed <- vec
    for (i in seq_along(vec)) {
      if (should_transform(vec[i])) {
        transformed[i] <- .single_varying_eff_inla(
          covariate = vec[i],
          pattern = pattern,
          name = name,
          model = model,
          unit = unit
        )
      }
    }
    list(original = vec, transformed = transformed)
  }
  
  result_pairs <- lapply(cov_list, transform_set)
  originals <- lapply(result_pairs, `[[`, "original")
  transformed <- lapply(result_pairs, `[[`, "transformed")
  
  # 6) Combine output ----
  if (add) {
    all_sets <- c(originals, transformed)
    keys <- unique(lapply(all_sets, function(x) paste(sort(x), collapse = "|")))
    result <- lapply(keys, function(k) unlist(strsplit(k, "\\|")))
  } else {
    result <- transformed
  }
  
  return(result)
}

#' Retrieve Covariates from a `GHRmodels` Object as a List of Character Vectors
#'
#' @description
#' Extracts covariates from a `GHRmodels` object and returns them as a list of character vectors. 
#' If `unique = TRUE`, the output contains unique covariates across models. If `unique = FALSE`, 
#' the output preserves the original combinations of covariates as specified in the `GHRmodels` object. 
#'
#' @param model A `GHRmodels` object containing fitted models. 
#'
#' @param unique Logical; if \code{TRUE}, returns unique covariates across models. 
#'   If \code{FALSE}, returns vectors of covariate combinations as declared in the `GHRmodels` object. 
#'
#' @return A list of character vectors.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load example dataset
#' data(dengueMS)
#' 
#' # Declare formulas
#' formulas <- c(
#'   "dengue_cases ~ tmin +  f(year, model='rw1')",
#'   "dengue_cases ~ pdsi +  f(year, model='rw1')"
#' )
#' 
#' # Tranform formulas into a 'GHRformulas' object
#' ghr_formulas <- as_GHRformulas(formulas)
#' 
#' # Fit multiple models 
#' results <- fit_models(
#'   formulas = ghr_formulas,
#'   data     = dengue_MS,
#'   family   = "nbinomial",
#'   name     = "TestModel",
#'   offset   = "population",
#'   nthreads = 2,
#'   control_compute = list(config = FALSE),
#'   pb       = TRUE
#' )
#' 
#' # Extract the list of covariates from the models 
#' get_covariates(results)
#'}

get_covariates <- function(model, unique = TRUE) {
  
  # 1) Checks ----
  if (missing(model)) {
    stop("'model' must be provided.")
  }
  if (!inherits(model, "GHRmodels")) {
    stop("'model' must be an object of class 'GHRmodels'.")
  }
  if (!is.logical(unique) || length(unique) != 1) {
    stop("'unique' must be a single logical value.")
  }
  
  # 2) Extract Covariates from the Mod Gof and  retranform them into inla.group ----
  mod_gof <- model$mod_gof
  
  covariates <- mod_gof |>
    dplyr::select(dplyr::starts_with("covariate"))
  
  covariates <- apply(X = covariates,
                      MARGIN = 2,
                      FUN = .join_inla_group)
  
  out <- apply(X = covariates,
               MARGIN = 1, 
               FUN = as.list)
  
  out <- lapply(out, function(x) unname(unlist(x[!sapply(x, is.na)])))
  out <- Filter(function(x) !(is.null(x)), out)
  
  # 3) Remove duplicated if unique is TRUE ----
  
  out <- if (unique == TRUE) {
    out <- as.list(unique(unlist(out)))
  } else {out <- out}
  
  # 4) Return the output ----
  return(out)
  }


#' Add Covariates to All Combinations
#'
#' @description
#'This function appends one or more covariate names to all elements (i.e., covariate sets)
#' in a list of character vectors. This is useful when a covariate (like a confounder or
#' control variable) needs to be included in every model. It also works with a single 
#' character vector input.  The resulting list 
#' can be input into the `covariates` argument in \code{\link{write_inla_formulas}}.
#'
#' @param covariates A character vector or a list of character vectors, where each vector 
#' represents a set of covariates (e.g., from \code{\link{cov_multi}}).
#' @param name A character vector of covariate names to be added to each set.
#' @param add Boolean that indicates if the original combinations in the `covariates`
#' argument must be kept. Defaults to FALSE.
#' @return A list of character vectors, with each vector containing the original covariates
#' plus the additional ones specified in the `name` argument.
#'
#' @export
#'
#' @examples
#' # Multiple combinations
#' cov_sets <- list(
#'   c("tmin", "pdsi"),
#'   c("tmin.l1", "pdsi"),
#'   c("tmin.l2", "pdsi")
#' )
#' cov_add(cov_sets, name = "urban")
#'

cov_add <- function(covariates, name, add = FALSE) {
  
  # 1) Checks ----
  if (is.null(covariates) || (!is.character(covariates) && !is.list(covariates))) {
    stop("'covariates' must be a character vector or a list of character vectors.")
  }
  if (!is.character(name)) {
    stop("'name' must be a character vector.")
  }
  if (!is.logical(add) || length(add) != 1) {
    stop("'add' must be a single logical value (TRUE or FALSE).")
  }
  
  # 2) Wrap single character vector into a list ----
  combos_list <- if (is.character(covariates)) list(covariates) else covariates
  if (!all(sapply(combos_list, is.character))) {
    stop("All elements of 'covariates' must be character vectors.")
  }
  
  # 3) Create list with added covariates ----
  added_list <- lapply(combos_list, function(x) unique(c(x, name)))
  
  # 4) Combine original + added (if add = TRUE) ----
  if (add) {
    combined <- c(combos_list, added_list)
    # Deduplicate by content (order insensible deduplication)
    unique_keys <- unique(lapply(combined, function(x) paste(sort(x), collapse = "|")))
    result <- lapply(unique_keys, function(k) unlist(strsplit(k, "\\|")))
  } else {
    result <- added_list
  }
  
  # 5) Return results ----
  return(result)
}


