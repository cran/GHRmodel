#_______________________________________________________________________________
# Helper Functions for processing R-INLA formulas ------------------------------
#_______________________________________________________________________________

# split_top_level ----------------------------------------------------------

#' Split a string on top-level occurrences of a separator
#'
#' Splits `x` on each occurrence of the single-character `sep` that is not
#' nested inside parentheses/brackets/braces. Used to split the right-hand
#' side of an INLA formula into its additive terms without breaking on a
#' literal separator that appears inside a term (e.g. in `f()` argument
#' lists, or scientific-notation hyperparameter values such as `1e+05`).
#'
#' Rather than hand-tracking quotes/parens character by character, this
#' relies on R's own tokenizer (via \code{\link[base]{parse}} with
#' \code{keep.source = TRUE} and \code{\link[utils]{getParseData}}) to
#' locate the separator tokens and their source positions; string literals
#' are therefore never mistaken for containing a top-level separator, since
#' the tokenizer already emits them as a single opaque token. The original
#' text is then sliced at those positions (not \code{deparse()}d), so
#' formatting (quote style, spacing) of each returned term is preserved
#' exactly as written.
#'
#' @param x A single character string.
#' @param sep A single character to split on (default `"+"`).
#'
#' @return A character vector of top-level substrings (not trimmed).
#' @noRd

.split_top_level <- function(x, sep = "+") {
  # Collapse embedded newlines
  x <- gsub("\r\n|\r|\n", " ", x)

  parsed <- tryCatch(
    parse(text = x, keep.source = TRUE),
    error = function(e) {
      stop("Could not parse formula right-hand side: ", x, call. = FALSE)
    }
  )

  # Leaf tokens (operators, literals, symbols) in source order; string
  # literals, function names, etc. are already single opaque tokens here.
  pd <- utils::getParseData(parsed)
  pd <- pd[pd$terminal & pd$line1 == pd$line2, ]
  pd <- pd[order(pd$col1), ]

  depth <- 0L
  split_after <- integer(0) # character offsets right after which to split
  for (i in seq_len(nrow(pd))) {
    tok <- pd$text[i]
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      depth <- depth - 1L
    } else if (tok == sep && depth == 0L) {
      split_after <- c(split_after, pd$col1[i])
    }
  }

  if (length(split_after) == 0) {
    return(x)
  }

  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  boundaries <- c(0L, split_after, length(chars) + 1L)
  tokens <- character(length(boundaries) - 1L)
  for (k in seq_len(length(boundaries) - 1L)) {
    start <- boundaries[k] + 1L
    end <- boundaries[k + 1L] - 1L
    tokens[k] <- if (start > end) {
      ""
    } else {
      paste(chars[start:end], collapse = "")
    }
  }
  tokens
}


# pad_to_df ------------------------------------------------------------------

#' Build a padded wide data.frame from a list of character vectors
#'
#' Combines a list of variable-length character vectors (one element per
#' formula) into a single data.frame: each vector becomes one row, padded
#' with `NA` up to the length of the longest vector in `values`, with
#' columns named `<prefix>1`, `<prefix>2`, ...
#'
#' @param values A list of character vectors (possibly of different
#' lengths, including zero-length).
#' @param prefix A character string used as the column-name prefix.
#'
#' @return A data.frame with `length(values)` rows. If every vector in
#' `values` is empty, the data.frame has zero columns; otherwise it has one
#' column per element up to the longest vector.
#' @noRd

.pad_to_df <- function(values, prefix) {
  max_len <- max(lengths(values))
  if (max_len == 0) {
    return(data.frame(matrix(nrow = length(values), ncol = 0)))
  }
  mat <- do.call(
    rbind,
    lapply(values, function(v) {
      length(v) <- max_len # Fill with NA if shorter
      v
    })
  )
  colnames(mat) <- paste0(prefix, seq_len(max_len))
  as.data.frame(mat, stringsAsFactors = FALSE)
}


#' Match covariates by pattern and/or exact name
#'
#' Shared matching logic for the `cov_*()`/`extract_names()` family in
#' `build_covariates.R`. Each `pattern` is matched either as a literal
#' prefix (`match_type = "prefix"`, via \code{\link[base]{startsWith}}) or
#' as a regular expression searched anywhere in the string
#' (`match_type = "regex"`, via \code{\link[base]{grep}}); `cov_multi()` is
#' the only caller that intentionally uses `"regex"`, everywhere else
#' `"prefix"` matches the documented "prefix" semantics. Each `name` is
#' matched exactly. A pattern/name that matches nothing in `covariates` is
#' recorded but does not stop execution here; callers decide how (or
#' whether) to error via \code{\link{.stop_if_missing}}.
#'
#' @param covariates Character vector to search within.
#' @param pattern Character vector of patterns, or `NULL`.
#' @param name Character vector of exact names, or `NULL`.
#' @param match_type Either `"prefix"` or `"regex"`.
#'
#' @return A list with elements: `groups` (a list of character vectors, one
#'   per matched pattern/name, in that order), `matched` (the unique union
#'   of `groups`, in first-matched order), `missing_patterns`, and
#'   `missing_names`.
#' @noRd
.match_pattern_name <- function(
  covariates,
  pattern = NULL,
  name = NULL,
  match_type = c("prefix", "regex")
) {
  match_type <- match.arg(match_type)

  groups <- list()
  missing_patterns <- c()
  missing_names <- c()

  if (!is.null(pattern)) {
    for (pat in pattern) {
      matched <- if (match_type == "regex") {
        grep(pat, covariates, value = TRUE)
      } else {
        covariates[startsWith(covariates, pat)]
      }
      if (length(matched) > 0) {
        groups[[length(groups) + 1]] <- matched
      } else {
        missing_patterns <- c(missing_patterns, pat)
      }
    }
  }

  if (!is.null(name)) {
    for (nm in name) {
      if (nm %in% covariates) {
        groups[[length(groups) + 1]] <- nm
      } else {
        missing_names <- c(missing_names, nm)
      }
    }
  }

  list(
    groups = groups,
    matched = unique(unlist(groups, use.names = FALSE)),
    missing_patterns = missing_patterns,
    missing_names = missing_names
  )
}

#' Stop if any requested pattern/name failed to match
#'
#' Companion to \code{\link{.match_pattern_name}}: raises the shared
#' "No matches found for: ..." error used across the `cov_*()` family
#' whenever at least one requested pattern or name matched nothing.
#'
#' @param missing_patterns Character vector of unmatched patterns.
#' @param missing_names Character vector of unmatched names.
#'
#' @return Invisibly `NULL`; called for its side effect.
#' @noRd
.stop_if_missing <- function(missing_patterns, missing_names) {
  if (length(missing_patterns) > 0 || length(missing_names) > 0) {
    stop(
      paste0(
        "No matches found for: ",
        paste(c(missing_patterns, missing_names), collapse = ", ")
      )
    )
  }
  invisible(NULL)
}

#' Deduplicate a list of covariate-set vectors, order-insensitively
#'
#' Shared `add = TRUE` dedup logic for `cov_nl()`, `cov_varying()`, and
#' `cov_add()`: two vectors containing the same elements in a different
#' order are treated as the same combination and only one is kept. Sets
#' are compared via a sorted "|"-joined key, so as a side effect each
#' returned vector has its elements in sorted order (this matches the
#' pre-refactor behaviour, which built the same key).
#'
#' @param sets A list of character vectors (covariate combinations).
#'
#' @return A list of character vectors with order-insensitive duplicates
#'   removed.
#' @noRd
.dedup_combo_list <- function(sets) {
  keys <- unique(vapply(
    sets,
    function(x) paste(sort(x), collapse = "|"),
    character(1)
  ))
  lapply(keys, function(k) unlist(strsplit(k, "\\|")))
}


# write_re_inla ----------------------------------------------------------------

#' Write Random effect for R-INLA
#'
#' Generates an INLA formula for specified random effects based on provided
#' parameters.
#'
#' @param id Character string. The name of the variable representing the units
#' for which the random effects should be specified.
#' @param model Character string. Defines the prior for the random effects.
#' Accepted values are "iid", "rw1", "rw2", "bym", "bym2".
#' @param replicate (Optional) Character string. The name of the variable for
#' which the random effect should be replicated.
#' @param group (Optional) Character string. The name of the variable for which
#' the random effect should be grouped.
#' @param graph (Optional) Character string. The name of the graph with the
#' adjacency matrix in case a spatially structured random effect is specified
#' (e.g., "bym", "bym2").
#' @param cyclic (Optional) Logical. If `TRUE`, the random effect is cyclical
#' (only for "rw1" and "rw2"). Default is `FALSE`.
#' @param scale.model (Optional) Logical. If `TRUE`, scales the random effects
#' so that the generalized variance = 1. This scaling applies to structured
#' random effects (e.g., Besag models, RW1, RW2, AR1), but not to IID random
#' effects. Default is `FALSE`.
#' @param constr (Optional) Logical. If `TRUE`, a sum to zero constrain is
#' introduced. This 'constr' option is applied only to 'iid' random effects.
#' If `FALSE`, default INLA constrains are applied. Default is `FALSE`.
#' @param adjust.for.con.comp (Optional) Logical. If `TRUE`, adjusts for
#' in models with spatial or temporal dependencies
#' (e.g., "rw1", "rw2", "bym", "bym2"). This adjustment is not applicable to
#' IID random effects. Default is `FALSE`
#' @param hyper (Optional) Character string. The name of the hyperparameter
#' list used in the INLA formula.
#'
#' @return A character string representing the INLA formula for the random
#' effect.
#' @noRd

.write_re_inla <- function(
  id,
  model,
  replicate = NULL,
  group = NULL,
  graph = NULL,
  cyclic = FALSE,
  scale.model = FALSE,
  constr = FALSE,
  adjust.for.con.comp = FALSE,
  hyper = NULL
) {
  # Check minimum required arguments
  if (missing(model) || missing(id)) {
    stop("'model' and 'id' parameters must be provided.")
  }

  # Allowed Random effects
  allowed_model <- c("iid", "rw1", "rw2", "bym", "bym2")
  if (!(model %in% allowed_model)) {
    stop(paste(
      "Invalid 'model' value. Accepted values are:",
      paste(allowed_model, collapse = ", ")
    ))
  }

  # Check if graph is present when random effects are spatially structured
  if ((model == "bym" || model == "bym2") && is.null(graph)) {
    stop("'graph' is missing")
  }

  # Check graph is character
  if ((model == "bym" || model == "bym2") && !is.character(graph)) {
    stop(
      "'graph' must be a single character string representing the ",
      "adjacency matrix name."
    )
  }

  # Start building the INLA formula
  re_inla <- paste0("f(", id, ", model = '", model, "'")

  # Add replicate parameter if provided
  if (!is.null(replicate)) {
    re_inla <- paste0(re_inla, ", replicate = ", replicate)
  }

  # Add group parameter if provided
  if (!is.null(group)) {
    re_inla <- paste0(re_inla, ", group = ", group)
  }

  # Add cyclic parameter if provided
  if (cyclic == TRUE && model %in% c("rw1", "rw2")) {
    re_inla <- paste0(re_inla, ", cyclic = TRUE")
  }

  # Add constr parameter if provided
  if (constr == TRUE && model %in% c("iid")) {
    re_inla <- paste0(re_inla, ", constr = TRUE")
  }

  # Add graph parameter for spatial models
  if (!is.null(graph)) {
    re_inla <- paste0(re_inla, ", graph = ", graph)
  }

  # Add scale.model parameter if provided
  if (scale.model == TRUE) {
    re_inla <- paste0(re_inla, ", scale.model = TRUE")
  }

  # Add adjust.for.con.comp parameter if provided
  if (
    model %in% c("rw1", "rw2", "bym", "bym2") && adjust.for.con.comp == TRUE
  ) {
    re_inla <- paste0(re_inla, ", adjust.for.con.comp = TRUE")
  }

  # Add hyperparameters if provided
  if (!is.null(hyper)) {
    # Ensure hyper is a character string representing the name of the hyper list
    if (!is.character(hyper) || length(hyper) != 1) {
      stop(
        "'hyper' must be a single character string representing the name of ",
        "the hyperparameter list (e.g 'hyper_list')"
      )
    }
    re_inla <- paste0(re_inla, ", hyper = ", hyper)
  }

  # Close the formula
  re_inla <- paste0(re_inla, ")")

  return(re_inla)
}


# classify_f_term ----------------------------------------------------------

#' Classify a single `f(...)` formula term
#'
#' Determines whether an INLA `f(...)` term is a standard random effect, a
#' spatially/temporally varying coefficient (SVC), or a non-linear
#' (`inla.group()`) covariate transform, based on the number of unnamed
#' (positional) arguments (i.e., random effect indices).
#'
#' @param term A single character string containing an `f(...)` term.
#'
#' @return A list with `$type` (one of `"re"`, `"svc"`, `"nl"`) and `$value`
#' (the term, or its non-linear label if `$type == "nl"`).
#' @noRd

.classify_f_term <- function(term) {
  parsed <- tryCatch(str2lang(term), error = function(e) NULL)
  if (is.null(parsed) || !is.call(parsed)) {
    stop(
      "Could not parse the random-effect/varying-coefficient term: ",
      term,
      call. = FALSE
    )
  }

  # Get unnamed arguments of f() (i.e, the RE indices)
  args <- as.list(parsed)[-1] # drop the leading `f` symbol
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  unnamed <- args[arg_names == ""]

  # In INLA the second positional argument of f() is `weights`, so
  # f(unit, covariate) and f(unit, weights = covariate) are equivalent
  has_weights <- length(unnamed) >= 1 && "weights" %in% arg_names
  if (length(unnamed) >= 2 || has_weights) {
    # f(unit, covariate, model = ..., ...) -> varying coefficient
    return(list(type = "svc", value = term))
  }

  if (length(unnamed) == 1) {
    first_arg <- unnamed[[1]]
    is_nl <- is.call(first_arg) &&
      grepl("(^|::)inla\\.group$", deparse(first_arg[[1]]))
    if (is_nl) {
      return(list(type = "nl", value = .split_inla_group(term)))
    }
    return(list(type = "re", value = term))
  }

  stop(
    "Random-effect term is missing an identifier variable: ",
    term,
    call. = FALSE
  )
}


# extract_formula_vars -----------------------------------------------------

#' Extract the data variables used by a single formula
#'
#' Parses the right-hand side of a formula with R's parser (not with string
#' matching) and returns the names of the data columns used by its linear
#' terms (covariates, interactions, transformations, `offset()`, ...). `f()`
#' terms (random effects, varying coefficients and non-linear effects) are
#' ignored.
#'
#' @param formula A single character string containing a formula.
#'
#' @return A character vector of variable names.
#' @noRd

.extract_formula_vars <- function(formula) {
  parts <- strsplit(formula, "~", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    stop("Each formula must contain exactly one '~'.", call. = FALSE)
  }
  terms <- trimws(.split_top_level(trimws(parts[2]), "+"))

  fixed <- character(0)

  for (term in terms) {
    # Same intercept handling as as_GHRformulas()
    minus_parts <- .split_top_level(term, "-")
    if (length(minus_parts) == 2 && trimws(minus_parts[2]) == "1") {
      term <- trimws(minus_parts[1])
    }
    if (term == "" || term == "1") {
      next
    }

    parsed <- tryCatch(str2lang(term), error = function(e) NULL)
    if (is.null(parsed)) {
      stop("Could not parse the formula term: ", term, call. = FALSE)
    }

    # f() terms are not checked
    if (grepl("^f\\s*\\(", term)) {
      next
    }

    # Linear terms, interactions, transformations, offset(), ...
    fixed <- c(fixed, all.vars(parsed))
  }

  unique(fixed)
}


# check_formula_vars -------------------------------------------------------

#' Check that linear covariates exist in the data and have no NAs
#'
#' Only the linear terms of the formulas are checked; `f()` terms are ignored.
#' Terms that are not columns of `data` are accepted if an object with that
#' name exists in the global environment (e.g., the basis matrices created with
#' [onebasis_inla()] or [crossbasis_inla()]), which R-INLA finds in the
#' environment of the formula. These are not checked for missing values, as
#' lagged bases have them by construction.
#'
#' @param formulas A character vector of formulas.
#' @param data A data.frame.
#'
#' @return Invisibly `NULL`; errors on missing variables or covariates with
#' missing values.
#' @noRd

.check_formula_vars <- function(formulas, data) {
  fixed <- unique(unlist(lapply(formulas, .extract_formula_vars)))

  # Objects living in the global environment (e.g., DLNM basis matrices)
  not_in_data <- setdiff(fixed, names(data))
  in_env <- vapply(
    not_in_data,
    exists,
    logical(1),
    envir = globalenv(),
    inherits = FALSE
  )
  fixed <- setdiff(fixed, not_in_data[in_env])

  missing_vars <- setdiff(fixed, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "The following variables used in the formulas have not been found ",
      "in the data: ",
      paste(missing_vars, collapse = ", "),
      "."
    )
  }

  has_na <- fixed[vapply(data[fixed], anyNA, logical(1))]
  if (length(has_na) > 0) {
    stop(
      "The following covariates used in the formulas contain missing ",
      "values: ",
      paste(has_na, collapse = ", "),
      "."
    )
  }

  invisible(NULL)
}


# single_non_linear_eff_inla ---------------------------------------------------

#' Transform variable names as non-linear call for R-INLA
#'
#' Generates INLA formulas for specified predictors to be modeled as non-linear
#' effects. This function applies the `inla.group` function to discretize
#' continuous covariates using specified methods and constructs the
#' corresponding INLA random effect formulas.
#'
#' @param covariate A character vector or a list of character vectors indicating
#' the variables to be expressed as non-linear terms in INLA.
#' @param pattern A character vector with the string patterns pointing the
#' variable names needed to be expressed as non-linear terms in INLA.
#' @param name A carachter specifying the name of the variable to be tranformed.
#' @param model Character string. Defines the model for the non-linear effect.
#' Accepted values are "rw1" (random walk of order 1) and "rw2" (random walk of
#' order 2).
#' @param method Character string. Specifies the method to discretize the linear
#' covariate. Accepted values are "cut" and "quantile".
#' @param n Numeric. The number of breaks to be used in the discretization
#' method. Must be a positive integer. Default is 10.
#' @param replicate Character, indicating the variable for which replication
#' of the non linear effect is needed.
#'
#' @return A character vector or matrix of predictors re-expressed as non-linear
#' terms for INLA models. Each element follows the format:
#' \code{f(INLA::inla.group(variable, method = 'method', n = n), model = 'model')}
#' @noRd

.single_non_linear_eff_inla <- function(
  covariate,
  pattern = NULL,
  name = NULL,
  model = "rw2",
  method = "quantile",
  n = 10,
  replicate = NULL
) {
  # Do not apply transformation to interaction terms
  if (grepl(":", covariate, fixed = TRUE)) {
    return(covariate)
  }

  # Validate inputs
  if (is.null(pattern) && is.null(name)) {
    stop("At least one of 'pattern' or 'name' must be provided.")
  }
  if (!is.null(pattern) && !is.character(pattern)) {
    stop("'pattern' must be a character vector if provided.")
  }
  if (!is.null(name) && !is.character(name)) {
    stop("'name' must be a character vector if provided.")
  }

  allowed_models <- c("rw1", "rw2")
  if (!(model %in% allowed_models)) {
    stop(paste(
      "Invalid 'model' value. Accepted values are:",
      paste(allowed_models, collapse = ", ")
    ))
  }
  allowed_methods <- c("cut", "quantile")
  if (!(method %in% allowed_methods)) {
    stop(paste(
      "Invalid 'method' value. Accepted values are:",
      paste(allowed_methods, collapse = ", ")
    ))
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a single positive integer.")
  }
  if (!is.null(replicate) && !is.character(replicate)) {
    stop("'replicate' must be a character vector or NULL.")
  }

  # Check matching logic
  pattern_match <- if (!is.null(pattern)) {
    any(sapply(pattern, function(p) startsWith(covariate, p)))
  } else {
    FALSE
  }
  name_match <- if (!is.null(name)) covariate %in% name else FALSE

  if (pattern_match || name_match) {
    method_part <- paste0("method='", method, "', n=", n)

    if (!is.null(replicate)) {
      return(
        paste0(
          "f(INLA::inla.group(",
          covariate,
          ", ",
          method_part,
          "), model='",
          model,
          "', replicate=",
          replicate,
          ")"
        )
      )
    } else {
      return(
        paste0(
          "f(INLA::inla.group(",
          covariate,
          ", ",
          method_part,
          "), model='",
          model,
          "')"
        )
      )
    }
  } else {
    return(covariate)
  }
}

#' Write a Spatially Varying INLA Effect Term
#'
#' @description
#' `single_varying_eff_inla` creates a character string representing
#' a spatially varying coefficient term for use in an INLA formula. It takes a
#' covariate name (or pattern), a spatial unit (which indexes the random
#' effect), and a model specification, and returns a string in the format:
#' \code{f(unit, covariate, model = "model")}.
#'
#' @param pattern A character string specifying the covariate pattern to be
#' used as the effect.
#' @param name A character string specifying the exact covariate name to be
#' used as the effect.
#' @param unit A character string specifying the spatial unit (e.g. "spat_id")
#' that indexes the spatially varying effect.
#' @param model A character string specifying the INLA model for the spatially
#' varying coefficient. Default is `"iid"`.
#'
#' @return A character string representing the spatially varying INLA effect
#' term.
#'
#' @details
#' The returned string can be inserted directly into an INLA formula to model
#' spatially varying coefficients.
#'
#' @noRd
#'
.single_varying_eff_inla <- function(
  covariate,
  unit,
  pattern = NULL,
  name = NULL,
  constr = FALSE,
  model = "iid"
) {
  # 0. Do not apply transformation to interacting covariates
  if (grepl(":", covariate, fixed = TRUE)) {
    return(covariate)
  }

  # 1. Validate single covariate
  if (
    missing(covariate) || !is.character(covariate) || length(covariate) != 1
  ) {
    stop("'covariate' must be a single character string.")
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

  allowed_models <- c("iid")
  if (!(model %in% allowed_models)) {
    stop(paste(
      "Invalid 'model' value. Accepted values are:",
      paste(allowed_models, collapse = ", ")
    ))
  }

  # 2. Check for match
  pattern_match <- if (!is.null(pattern)) {
    any(sapply(pattern, function(p) startsWith(covariate, p)))
  } else {
    FALSE
  }
  name_match <- if (!is.null(name)) covariate %in% name else FALSE

  # 3. Build f() term with constr = TRUE/FALSE (uppercase!)
  if (pattern_match || name_match) {
    constr_part <- paste0(
      ", constr = ",
      if (isTRUE(constr)) "TRUE" else "FALSE"
    )
    return(
      paste0(
        "f(",
        unit,
        ", ",
        covariate,
        ", model = '",
        model,
        "'",
        constr_part,
        ")"
      )
    )
  } else {
    return(covariate)
  }
}


#' Extract Information from Non-Linear R-INLA Calls
#'
#' Parses `inla.group()` function calls to retrieve the covariate name,
#' method, model, and replication variable in **INLA** models.
#'
#' @param term A string containing an `inla.group()` function call in the
#' format: \code{f(INLA::inla.group(variable, method = 'method', n = n),
#' model = 'model', replicate = group_var)}
#'
#' @return A **short-form** label string summarizing the non-linear
#' transformation. The format of the returned string is:
#'   \preformatted{
#'   "var_nl_[method][n]_[model]_rep_[rep_variable]"
#'   }
#'   where `_rep_[rep_variable]` is appended **only if the transformation
#'   includes a `replicate` argument**.
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' term <- "f(INLA::inla.group(tmin.l2, method='quantile', n=10),
#'                             model='rw2', replicate=urban)"
#' split_inla_group(term)
#' }
.split_inla_group <- function(term) {
  # Return NA if the term itself is NA
  if (is.na(term)) {
    return(term)
  }

  # Parse the term so that argument names, positions, quote style
  # and nested calls do not matter
  parsed <- tryCatch(str2lang(term), error = function(e) NULL)
  if (is.null(parsed) || !is.call(parsed)) {
    stop("Could not parse the non-linear term: ", term, call. = FALSE)
  }
  f_args <- as.list(parsed)[-1]
  f_names <- names(f_args)
  if (is.null(f_names)) {
    f_names <- rep("", length(f_args))
  }

  # The inla.group() call is the first positional argument of f()
  ig_call <- f_args[f_names == ""][[1]]
  ig_args <- as.list(ig_call)[-1]
  ig_names <- names(ig_args)
  if (is.null(ig_names)) {
    ig_names <- rep("", length(ig_args))
  }

  # Match inla.group() arguments as R does: named ones first (with partial
  # matching), then positional ones fill the remaining formals in order
  formal_names <- c("x", "n", "method", "idx.only")
  matched <- vector("list", length(formal_names))
  names(matched) <- formal_names
  for (i in which(ig_names != "")) {
    idx <- pmatch(ig_names[i], formal_names)
    if (!is.na(idx)) {
      matched[[idx]] <- ig_args[[i]]
    }
  }
  free <- formal_names[vapply(matched, is.null, logical(1))]
  positional <- ig_args[ig_names == ""]
  for (i in seq_len(min(length(free), length(positional)))) {
    matched[[free[i]]] <- positional[[i]]
  }

  if (is.null(matched$x)) {
    stop("inla.group() has no variable in term: ", term, call. = FALSE)
  }

  # Arguments that go into the label must be literals, otherwise they would
  # be silently lost
  as_literal <- function(arg, what, check) {
    if (is.null(arg)) {
      return(NA_character_)
    }
    if (!check(arg)) {
      stop(
        "Cannot use a non-literal '",
        what,
        "' in the non-linear term: ",
        term,
        call. = FALSE
      )
    }
    if (is.numeric(arg)) format(arg, scientific = FALSE) else as.character(arg)
  }

  var_name <- paste(deparse(matched$x), collapse = "")
  method <- as_literal(matched$method, "method", is.character)
  n_val <- as_literal(matched$n, "n", is.numeric)
  model_val <- as_literal(f_args[["model"]], "model", is.character)

  # Replication variable (any expression, e.g. `urban` or `as.integer(urban)`)
  replicate_val <- if (is.null(f_args[["replicate"]])) {
    NA_character_
  } else {
    paste(deparse(f_args[["replicate"]]), collapse = "")
  }

  # Construct final label: var_nl_[method][n]_[model]
  # rep_[rep_variable] (if replicate exists)
  out <- paste0(var_name, "_nl") # Always add "_nl"
  if (!is.na(method)) {
    out <- paste0(out, "_", substr(method, 1, 1))
  } # First letter of method
  if (!is.na(n_val)) {
    out <- paste0(out, n_val)
  } # Append n (e.g., "_c10")
  if (!is.na(model_val)) {
    out <- paste0(out, "_", model_val)
  } # Append model (e.g., "_rw2")
  if (!is.na(replicate_val)) {
    out <- paste0(out, "_rep_", replicate_val)
  } # Append "_rep_[rep_variable]" if replicated

  return(out)
}

#' Re-create an INLA non-linear term from a compact label
#'
#' Converts labels produced by `.split_inla_group()` back into the full
#' `f(INLA::inla.group())` call.
#' **If the label does not contain `"_nl"`, it is returned as-is.**
#'
#' @param label Character vector of labels such as
#'              `"temp_nl_q10_rw2_rep_site"` or `"rain_nl_rw1"`.
#'
#' @return A character vector with the reconstructed call(s), or the original
#'         string when no non-linear suffix is present.
#' @noRd

.join_inla_group <- function(label) {
  ## -- helper : map abbreviation → full method name ---------------------------
  map_method <- function(abbr) {
    abbr <- tolower(abbr)
    full <- c("cut", "quantile", "equal")
    hit <- full[startsWith(full, abbr)]
    if (length(hit)) hit[1L] else abbr
  }

  reconstruct_one <- function(lbl) {
    if (is.na(lbl) || !grepl("_nl", lbl, fixed = TRUE)) {
      return(lbl)
    } # <-- nothing to do

    ## 1) separate optional "_rep_<var>"
    rep_pos <- regexpr("_rep_", lbl, fixed = TRUE)
    if (rep_pos != -1) {
      replicate_var <- substr(lbl, rep_pos + 5L, nchar(lbl))
      core <- substr(lbl, 1L, rep_pos - 1L)
    } else {
      replicate_var <- NA_character_
      core <- lbl
    }

    ## 2) split "<var>_nl(_something)?"
    m <- regexec("^(.*)_nl(?:_)?(.*)$", core, perl = TRUE)
    parts <- regmatches(core, m)[[1L]]
    ## (regex guaranteed to match because "_nl" confirmed above)
    var_name <- parts[2L]
    rest <- parts[3L]
    tokens <- if (nzchar(rest)) strsplit(rest, "_")[[1L]] else character()

    ## 3) parse tokens : method+n and model
    method_abbr <- n_val <- model_val <- NA_character_

    if (length(tokens)) {
      first <- tokens[1L]
      letters_only <- sub("\\d.*$", "", first)
      digits_only <- sub("^[A-Za-z]+", "", first)

      if (
        tolower(letters_only) %in%
          c("c", "cu", "cut", "q", "qu", "qua", "quantile", "e", "eq", "equal")
      ) {
        method_abbr <- letters_only
        n_val <- if (nchar(digits_only)) digits_only else NA_character_
        tokens <- tokens[-1L]
      } else if (grepl("^[0-9]+$", first)) {
        # n without method, e.g. "tmin_nl10_rw2"
        n_val <- first
        tokens <- tokens[-1L]
      }
    }
    if (length(tokens)) {
      model_val <- tokens[1L]
    }

    ## 4) build the call string
    ig_args <- var_name
    if (!is.na(method_abbr)) {
      ig_args <- paste0(ig_args, ", method = '", map_method(method_abbr), "'")
    }
    if (!is.na(n_val)) {
      ig_args <- paste0(ig_args, ", n = ", n_val)
    }

    call <- paste0("f(INLA::inla.group(", ig_args, ")")

    extra <- character()
    if (!is.na(model_val)) {
      extra <- c(extra, paste0("model = '", model_val, "'"))
    }
    if (!is.na(replicate_var)) {
      extra <- c(extra, paste0("replicate = ", replicate_var))
    }

    if (length(extra)) {
      call <- paste0(call, ", ", paste(extra, collapse = ", "))
    }

    paste0(call, ")")
  }

  vapply(label, reconstruct_one, FUN.VALUE = character(1L), USE.NAMES = FALSE)
}


#' Create interaction terms
#'
#' Create interaction terms across variables
#'
#' @param matched_vars Identified covariate names.
#'
#' @return A character vector with the interaction terms.
#'
#' @noRd

.build_interaction <- function(matched_vars) {
  if (length(matched_vars) == 2) {
    paste0(matched_vars[1], ":", matched_vars[2])
  } else {
    a12 <- paste0(matched_vars[1], ":", matched_vars[2])
    a13 <- paste0(matched_vars[1], ":", matched_vars[3])
    a23 <- paste0(matched_vars[2], ":", matched_vars[3])
    a123 <- paste0(matched_vars[1], ":", matched_vars[2], ":", matched_vars[3])

    c(a12, a13, a23, a123)
  }
}

#  expand_interaction ---------------------------------------------------------

#' Expand Interaction terms
#'
#' Expands chunk of INLA formulas with multiple '*' into main effects &
#' interactions
#' @param term a string including interaction operator '*'.
#' @return A a vector with all interacting variables
#' @noRd

.expand_interaction <- function(term) {
  # Split by '*'
  factors <- unlist(strsplit(term, "\\*"))
  factors <- trimws(factors)

  # If there is only one factor, return it unchanged
  if (length(factors) == 1) {
    return(factors)
  }

  out <- character(0)

  # Build all combinations (from main effects up to full interaction)
  for (k in seq_along(factors)) {
    comb_k <- utils::combn(factors, k)
    # Combine each combination into a colon-separated term (e.g., "var1:var2")
    expanded_terms <- apply(comb_k, 2, function(xx) paste(xx, collapse = ":"))
    out <- c(out, expanded_terms)
  }

  # Return unique terms in case of duplicates (unlikely but good practice)
  unique(out)
}
