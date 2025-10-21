#_______________________________________________________________________________
# Helper Functions for processing R-INLA formulas ------------------------------
#_______________________________________________________________________________


# write_re_inla ----------------------------------------------------------------

#' Write Random effect for R-INLA
#' 
#' Generates an INLA formula for specified random effects based on provided parameters.
#'
#' @param id Character string. The name of the variable representing the units for which
#'           the random effects should be specified.
#' @param model Character string. Defines the prior for the random effects.
#'           Accepted values are "iid", "rw1", "rw2", "bym", "bym2".
#' @param replicate (Optional) Character string. The name of the variable for which the random
#'            effect should be replicated.
#' @param group (Optional) Character string. The name of the variable for which the random
#'            effect should be grouped.
#' @param graph (Optional) Character string. The name of the graph with the adjacency matrix
#'              in case a spatially structured random effect is specified (e.g., "bym", "bym2").
#' @param cyclic (Optional) Logical. If `TRUE`, the random effect is cyclical (only for "rw1" and "rw2"). Default is `FALSE`.
#' @param scale.model (Optional) Logical. If `TRUE`, scales the 
#'     random effects so that the generalized variance = 1. 
#'     This scaling applies to structured random effects (e.g., Besag models, RW1, RW2, AR1),
#'     but not to IID random effects.
#'     Default is `FALSE`.
#' @param constr (Optional) Logical. If `TRUE`, a sum to zero constrain is introduced. 
#'     This 'constr' option is applied only to 'iid' random effects. If `FALSE`, default INLA
#'     constrains are applied. Default is `FALSE`.
#' @param adjust.for.con.comp (Optional) Logical. If `TRUE`, adjusts for 
#'     in models with spatial or temporal dependencies 
#'     (e.g., "rw1", "rw2", "bym", "bym2"). This adjustment is not applicable to 
#'     IID random effects. Default is `FALSE`
#'@param hyper (Optional) Character string. The name of the hyperparameter list used in the INLA formula.
#'
#' @return A character string representing the INLA formula for the random effect.
#' @noRd

.write_re_inla <- function(id,
                           model,
                           replicate = NULL,
                           group = NULL,
                           graph = NULL, 
                           cyclic = FALSE,
                           scale.model = FALSE,
                           constr = FALSE,
                           adjust.for.con.comp = FALSE, 
                           hyper = NULL) {
  
  # Check minimum required arguments 
  if (missing(model) || missing(id)) {
    stop("'model' and 'id' parameters must be provided.")
  }
  
  # Allowed Random effects 
  allowed_model <- c("iid", "rw1", "rw2", "bym", "bym2")
  if (!(model %in% allowed_model)) {
    stop(paste("Invalid 'model' value. Accepted values are:", paste(allowed_model, collapse = ", ")))
  }
  
  # Check if graph is present when random effects are spatially structured 
  if ((model == "bym" || model == "bym2") && is.null(graph)) {
    stop("'graph' is missing")}
  
  # Check graph is character 
  if ((model == "bym" || model == "bym2") && !is.character(graph)) {
    stop("'graph' must be a single character string representing the adjacency matrix name.")}
  
  
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
  if (model %in% c("rw1", "rw2", "bym", "bym2") && adjust.for.con.comp == TRUE) {
    re_inla <- paste0(re_inla, ", adjust.for.con.comp = TRUE")
  }

  # Add hyperparameters if provided
  if (!is.null(hyper)) {
    # Ensure hyper is a character string representing the name of the hyper list
    if (!is.character(hyper) || length(hyper) != 1) {
      stop("'hyper' must be a single character string representing the name of the hyperparameter list (e.g 'hyper_list')")
    }
    re_inla <- paste0(re_inla, ", hyper = ", hyper)
  }
  
  # Close the formula
  re_inla <- paste0(re_inla, ")")
  
  return(re_inla)
  
}


# single_non_linear_eff_inla ---------------------------------------------------

#' Transform variable names as non-linear call for R-INLA
#'
#' Generates INLA formulas for specified predictors to be modeled as non-linear effects.
#' This function applies the `inla.group` function to discretize continuous covariates
#' using specified methods and constructs the corresponding INLA random effect formulas.
#'
#' @param covariate A character vector or a list of character vectors indicating the variables
#'                   to be expressed as non-linear terms in INLA. 
#' @param pattern A character vector with the string patterns pointing the variable names needed to be expressed
#'                as non-linear terms in INLA. 
#' @param name A carachter specifying the name of the variable to be tranformed.
#' @param model Character string. Defines the model for the non-linear effect. 
#'              Accepted values are "rw1" (random walk of order 1) and "rw2" (random walk of order 2).
#' @param method Character string. Specifies the method to discretize the linear covariate.
#'               Accepted values are "cut" and "quantile".
#' @param n Numeric. The number of breaks to be used in the discretization method.
#'          Must be a positive integer. Default is 10.
#' @param replicate Character, indicating the variable for which replication of the non linear effect is needed
#'
#' @return A character vector or matrix of predictors re-expressed as non-linear terms for
#'         INLA models. Each element follows the format:
#'         \code{f(INLA::inla.group(variable, method = 'method', n = n), model = 'model')}
#' @noRd


.single_non_linear_eff_inla <- function(covariate,
                                        pattern = NULL,
                                        name = NULL,
                                        model = "rw2",
                                        method = "quantile",
                                        n = 10, 
                                        replicate = NULL) {
  
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
    stop(paste("Invalid 'model' value. Accepted values are:",
               paste(allowed_models, collapse = ", ")))
  }
  allowed_methods <- c("cut", "quantile")
  if (!(method %in% allowed_methods)) {
    stop(paste("Invalid 'method' value. Accepted values are:",
               paste(allowed_methods, collapse = ", ")))
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a single positive integer.")
  }
  if (!is.null(replicate) && !is.character(replicate)) {
    stop("'replicate' must be a character vector or NULL.")
  }
  
  # Check matching logic
  pattern_match <- if (!is.null(pattern)) any(sapply(pattern, function(p) startsWith(covariate, p))) else FALSE
  name_match <- if (!is.null(name)) covariate %in% name else FALSE
  
  if (pattern_match || name_match) {
    method_part <- paste0("method='", method, "', n=", n)
    
    if (!is.null(replicate)) {
      return(
        paste0(
          "f(INLA::inla.group(",
          covariate,
          ", ", method_part, "), model='", model, "', replicate=", replicate, ")"
        )
      )
    } else {
      return(
        paste0(
          "f(INLA::inla.group(",
          covariate,
          ", ", method_part, "), model='", model, "')"
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
#' `single_varying_eff_inla` creates a character string representing a spatially
#' varying coefficient term for use in an INLA formula. It takes a covariate name (or pattern),
#' a spatial unit (which indexes the random effect), and a model specification, and returns
#' a string in the format:
#' \code{f(unit, covariate, model = "model")}.
#'
#' @param pattern A character string specifying the covariate pattern to be used as the effect.
#' @param name A character string specifying the exact covariate name to be used as the effect.
#' @param unit A character string specifying the spatial unit (e.g. "spat_id") that indexes the spatially varying effect.
#' @param model A character string specifying the INLA model for the spatially varying coefficient.
#'   Default is `"iid"`.
#'
#' @return A character string representing the spatially varying INLA effect term.
#'
#' @details
#' The returned string can be inserted directly into an INLA formula to model spatially varying coefficients.
#'
#' @noRd
#'
#' 

.single_varying_eff_inla <- function(covariate,
                                     unit,
                                     pattern = NULL,
                                     name = NULL,
                                     constr = FALSE, 
                                     model = "iid") {
  
  # 0. Do not apply transformation to interacting covariates 
  if (grepl(":", covariate, fixed = TRUE)) {
    return(covariate)
  }  
  
  # 1. Validate single covariate
  if (missing(covariate) || !is.character(covariate) || length(covariate) != 1) {
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
    stop(paste("Invalid 'model' value. Accepted values are:",
               paste(allowed_models, collapse = ", ")))
  }
  
  # 2. Check for match
  pattern_match <- if (!is.null(pattern)) any(sapply(pattern, function(p) startsWith(covariate, p))) else FALSE
  name_match <- if (!is.null(name)) covariate %in% name else FALSE
  
  # 3. Build f() term with constr = TRUE/FALSE (uppercase!)
  if (pattern_match || name_match) {
    constr_part <- paste0(", constr = ", if (isTRUE(constr)) "TRUE" else "FALSE")
    return(
      paste0("f(", unit, ", ", covariate, ", model = '", model, "'", constr_part, ")")
    )
  } else {
    return(covariate)
  }
}




#' Extract Information from Non-Linear R-INLA Calls
#'
#' Parses `inla.group()` function calls to retrieve the covariate name, method, model, 
#' and replication variable in **INLA** models.
#'
#' @param term A string containing an `inla.group()` function call in the format:
#'   \code{f(INLA::inla.group(variable, method = 'method', n = n), model = 'model', replicate = group_var)}
#'
#' @return A **short-form** label string summarizing the non-linear transformation.
#'   The format of the returned string is:
#'   \preformatted{
#'   "var_nl_[method][n]_[model]_rep_[rep_variable]"
#'   }
#'   where `_rep_[rep_variable]` is appended **only if the transformation includes a `replicate` argument**.
#'
#' @noRd
#'
#' @examples
#' \donttest{
#' term <- "f(INLA::inla.group(tmin.l2, method='quantile', n=10), model='rw2', replicate=urban)"
#' split_inla_group(term)
#' }
.split_inla_group <- function(term) {
  
  x <- as.character(term)  # Ensure term is character
  
  # Return NA if the term itself is NA
  if (is.na(term)) return(term)
  
  # Extract the content inside f(...), ignoring leading "f(" and closing ")"
  inner <- sub("^f\\((.*)\\)$", "\\1", term)
  
  # Separate the `inla.group(...)` part from other arguments (like model='rw2', replicate='region')
  parts <- strsplit(inner, "\\),")[[1]]
  
  # First part should contain "INLA::inla.group(...)", other parts might contain model and replicate arguments
  inlagroup_part <- parts[1]
  other_part     <- if (length(parts) > 1) parts[2] else ""
  
  # Remove "INLA::inla.group(" prefix and trailing ")"
  inlagroup_part <- sub("^.*inla\\.group\\(", "", inlagroup_part)
  inlagroup_part <- sub("\\)$", "", inlagroup_part)
  
  # Extract variable name (everything before the first comma or entire string if no comma exists)
  first_comma_pos <- regexpr(",", inlagroup_part)
  if (first_comma_pos == -1) {
    var_name <- trimws(inlagroup_part)
  } else {
    var_name <- trimws(substr(inlagroup_part, 1, first_comma_pos - 1))
  }
  
  # Extract method (e.g., "method='cut'")
  method <- sub(".*method\\s*=\\s*'([^']+)'.*", "\\1", inlagroup_part)
  if (identical(method, inlagroup_part)) method <- NA_character_
  
  # Extract the number of cuts (n value)
  n_val <- sub(".*n\\s*=\\s*(\\d+).*", "\\1", inlagroup_part)
  if (identical(n_val, inlagroup_part)) n_val <- NA_character_
  
  # Extract model (e.g., "model='rw2'")
  model_val <- sub(".*model\\s*=\\s*'([^']+)'.*", "\\1", other_part)
  if (identical(model_val, other_part)) model_val <- NA_character_
  
  # Extract replication variable (e.g., "replicate=urban")
  replicate_val <- sub(".*replicate\\s*=\\s*([^,\\)]+).*", "\\1", other_part)
  if (identical(replicate_val, other_part)) replicate_val <- NA_character_
  
  # Construct final label: var_nl_[method][n]_[model]_rep_[rep_variable] (if replicate exists)
  out <- paste0(var_name, "_nl")  # Always add "_nl"
  if (!is.na(method)) out <- paste0(out, "_", substr(method, 1,1))  # First letter of method
  if (!is.na(n_val)) out <- paste0(out, n_val)   # Append n (e.g., "_c10")
  if (!is.na(model_val)) out <- paste0(out, "_", model_val)  # Append model (e.g., "_rw2")
  if (!is.na(replicate_val)) out <- paste0(out, "_rep_", replicate_val)  # Append "_rep_[rep_variable]" if replicated
  
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
    hit  <- full[startsWith(full, abbr)]
    if (length(hit)) hit[1L] else abbr
  }
  
  reconstruct_one <- function(lbl) {
    if (is.na(lbl) || !grepl("_nl", lbl, fixed = TRUE))
      return(lbl)                       # <-- nothing to do
    
    ## 1) separate optional "_rep_<var>"
    rep_pos <- regexpr("_rep_", lbl, fixed = TRUE)
    if (rep_pos != -1) {
      replicate_var <- substr(lbl, rep_pos + 5L, nchar(lbl))
      core          <- substr(lbl, 1L, rep_pos - 1L)
    } else {
      replicate_var <- NA_character_
      core          <- lbl
    }
    
    ## 2) split "<var>_nl(_something)?"
    m <- regexec("^(.*)_nl(?:_)?(.*)$", core, perl = TRUE)
    parts <- regmatches(core, m)[[1L]]
    ## (regex guaranteed to match because "_nl" confirmed above)
    var_name <- parts[2L]
    rest     <- parts[3L]
    tokens   <- if (nzchar(rest)) strsplit(rest, "_")[[1L]] else character()
    
    ## 3) parse tokens : method+n and model
    method_abbr <- n_val <- model_val <- NA_character_
    
    if (length(tokens)) {
      first <- tokens[1L]
      letters_only <- sub("\\d.*$", "", first)
      digits_only  <- sub("^[A-Za-z]+", "", first)
      
      if (tolower(letters_only) %in% c("c", "cu", "cut",
                                       "q", "qu", "qua", "quantile",
                                       "e", "eq", "equal")) {
        method_abbr <- letters_only
        n_val       <- if (nchar(digits_only)) digits_only else NA_character_
        tokens      <- tokens[-1L]
      }
    }
    if (length(tokens)) model_val <- tokens[1L]
    
    ## 4) build the call string
    ig_args <- var_name
    if (!is.na(method_abbr))
      ig_args <- paste0(ig_args,
                        ", method = '", map_method(method_abbr), "'")
    if (!is.na(n_val))
      ig_args <- paste0(ig_args, ", n = ", n_val)
    
    call <- paste0("f(INLA::inla.group(", ig_args, ")")
    
    extra <- character()
    if (!is.na(model_val))
      extra <- c(extra, paste0("model = '", model_val, "'"))
    if (!is.na(replicate_var))
      extra <- c(extra, paste0("replicate = ", replicate_var))
    
    if (length(extra))
      call <- paste0(call, ", ", paste(extra, collapse = ", "))
    
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
    a12  <- paste0(matched_vars[1], ":", matched_vars[2])
    a13  <- paste0(matched_vars[1], ":", matched_vars[3])
    a23  <- paste0(matched_vars[2], ":", matched_vars[3])
    a123 <- paste0(matched_vars[1], ":", matched_vars[2], ":", matched_vars[3])
    
    c(a12, a13, a23, a123)
  }
}

#  expand_interaction ---------------------------------------------------------

#' Expand Interaction terms
#' 
#' Expands chunk of INLA formulas with multiple '*' into main effects & interactions
#' @param term a string including interaction operator '*'.
#' @return A a vector with all interacting variables 
#' @noRd

.expand_interaction <- function(term) {
  # Split by '*'
  factors <- unlist(strsplit(term, "\\*"))
  factors <- trimws(factors)
  
  # If there is only one factor, return it unchanged
  if (length(factors) == 1) return(factors)
  
  out <- character(0)
  
  # Build all combinations (from main effects up to full interaction)
  for (k in seq_len(length(factors))) {
    comb_k <- utils::combn(factors, k)
    # Combine each combination into a colon-separated term (e.g., "var1:var2")
    expanded_terms <- apply(comb_k, 2, function(xx) paste(xx, collapse = ":"))
    out <- c(out, expanded_terms)
  }
  
  # Return unique terms in case of duplicates (unlikely but good practice)
  unique(out)
}



