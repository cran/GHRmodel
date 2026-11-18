#' Fit a single GHR model random effects model
#'
#' Fits a single GHR model for a given outcome and effect specification.
#'
#' @param data A data.frame containing all variables required to fit the model.
#' @param outcome Character string of length 1 giving the outcome variable
#' name used on the left-hand side of the model formula.
#' @param effect Named character vector of length 1 giving the model effect
#' (random or fixed) to be appended to the baseline formula (`outcome ~ 1`).
#' The name is used as the model identifier.
#' @param family Character string specifying the likelihood family passed to
#' \code{fit_models()} (default: \code{"nbinomial"}).
#' @param offset_var A character string specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param control_compute A named list of control options passed to
#' \code{fit_models()}, e.g. \code{list(config = FALSE, vcov = FALSE)}.
#' @param nthreads An integer specifying the number of threads for parallel
#' computation. Default is \code{1}.
#'
#' @return A data.frame containing goodness-of-fit metrics for the fitted model,
#'   with additional columns:
#'   \itemize{
#'     \item \code{model_id}: model identifier
#'     \item \code{effect}: effect specification used in the model
#'   }
#'
#' @seealso \code{\link{fit_models}}, \code{\link{as_GHRformulas}}
#'
#' @noRd
#'
.model_re <- function(
  data,
  outcome,
  effect,
  family = "nbinomial",
  offset_var = NULL,
  control_compute = list(config = FALSE, vcov = FALSE),
  nthreads = 1
) {
  if (missing(outcome) || !is.character(outcome) || length(outcome) != 1) {
    stop("'outcome' must be a single character string.")
  }

  if (length(effect) != 1) {
    stop("'effect' must be a named character vector of length 1.")
  }

  model_id <- names(effect)
  model_id <- ifelse(is.null(model_id) || model_id == "", "mod", model_id)

  plus_effect <- if (effect != "") paste0(" + ", effect) else ""
  GHRformulas <- as_GHRformulas(paste0(outcome, " ~ 1", plus_effect))

  mod_all <- fit_models(
    formulas = GHRformulas,
    data = data,
    family = family,
    name = model_id,
    offset_var = offset_var,
    control_compute = control_compute,
    nthreads = nthreads
  )

  return(mod_all)
}

#' Fit GHR models in batches and aggregate goodness-of-fit metrics
#'
#' Iterates over a set of model effects, fits each model using
#' \code{\link{.model_re}}, and aggregates goodness-of-fit (GOF)
#' metrics into a single data frame. Optional writing of model
#' objects and combined GOF metrics to disk.
#'
#' @param data A data.frame containing all variables required to fit the models.
#' @param outcome Character string of length 1 giving the outcome variable
#'   name used on the left-hand side of the model formula.
#' @param effect Named character vector of model effects (random or fixed)
#'   to be appended to the baseline formula (`outcome ~ 1`).
#' @param family Character string specifying the likelihood family passed to
#'   \code{fit_models()} (default: \code{"nbinomial"}).
#' @param offset_var A character string specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param control_compute A named list of control options passed to
#' \code{fit_models()}.
#' @param nthreads An integer specifying the number of threads for parallel
#' computation. Default is \code{1}.
#'
#' @return A dataframe with the combined goodness of fit metrics for all models
#'
#' @details
#' Each element of \code{formulas} is fitted separately.
#' Goodness-of-fit outputs returned by \code{\link{.model_re}} are row-bound.
#'Columns related to variance decomposition are removed upstream by
#' \code{.model_re()}.
#'
#' @seealso \code{\link{.model_re}}, \code{\link{fit_models}}
#'
#' @noRd
.model_batch <- function(
  data,
  outcome,
  effect,
  family = "nbinomial",
  offset_var = NULL,
  control_compute = list(config = FALSE, vcov = FALSE),
  nthreads = 1
) {
  mod_list <- vector("list", length(effect))
  gof_list <- vector("list", length(effect))

  # Iterate over effects
  for (i in seq_along(effect)) {
    tryCatch(
      expr = {
        mod_list[[i]] <- .model_re(
          data = data,
          outcome = outcome,
          effect = effect[i],
          family = family,
          offset_var = offset_var,
          control_compute = control_compute,
          nthreads = nthreads
        )
        gof_list[[i]] <- mod_list[[i]]$mod_gof |>
          dplyr::select(-dplyr::matches("vs_first|_var")) |>
          dplyr::mutate(
            model_id = model_id,
            effect = effect[i]
          )
      },
      error = function(e) {
        message(paste0(
          "INLA returned an error, model failed to run. Skipping model: ",
          names(effect[i])
        ))
      }
    )
  }

  # Parse outputs
  mod_list <- mod_list[!sapply(mod_list, is.null)]
  gof_list <- gof_list[!sapply(gof_list, is.null)]

  return(list(mod_list = mod_list, gof_list = gof_list))
}

#' Stack a list of GHRmodels objects, tolerating a single element
#'
#' Thin wrapper around \code{\link{stack_models}}: \code{stack_models()}
#' requires at least two 'GHRmodels' objects to stack, but a selection stage
#' can legitimately end up with only one fitted model (e.g. a single
#' random-effect group with a single prior). In that case there is nothing
#' to stack, so the lone element is returned as-is.
#'
#' @param mod_list A list of one or more 'GHRmodels' objects, as returned by
#' \code{\link{.model_batch}}'s \code{mod_list} element.
#'
#' @return A single 'GHRmodels' object.
#'
#' @seealso \code{\link{stack_models}}
#' @noRd
.stack_models_safe <- function(mod_list) {
  if (length(mod_list) == 1L) {
    return(mod_list[[1L]])
  }
  stack_models(mod_list)
}

#' Select top-ranked model effects by group
#'
#' Selects the best-performing model effects within each group based on a
#' ranking metric, returning a named list of selected effects
#'
#' For each group defined by `group_col`, the function keeps the top
#' `n_best` rows with the smallest values of `rank_col`, collects the
#' corresponding values of `effect_col`, and returns them as a named list.
#'
#' @param mod_gof A data frame or tibble containing model goodness-of-fit
#' metrics and grouping variables.
#' @param n_best Integer. Number of top-ranked models to retain per group.
#' Defaults to 3.
#' @param rank_col Character. Column name giving the ranking metric used to
#' select best models (lower values indicate better rank).
#' @param id_col Character. Column name giving the model_id.
#' @param group_col Character. Column name defining groups (e.g. covariates)
#' within which models are ranked.
#' @param effect_col Character. Column name giving the model effect identifiers
#' to be output.
#' @param group_value Character vector of group values to retain prior to
#' ranking (e.g. c("yearly", "monthly", "spatial")).
#'
#' @return
#' A named list where:
#' \itemize{
#'   \item each name corresponds to a unique value of `group_col`
#'   \item each element is a character vector of selected model effects
#'     from `effect_col`
#' }
#'
#' @details
#' The function uses tidy evaluation to allow flexible specification of
#' column names. Group values matching `group_value` are retained before
#' ranking is applied.
#'
#' @noRd

.select_effects <- function(
  mod_gof,
  n_best = 3,
  rank_col,
  id_col,
  group_col = NULL,
  effect_col = NULL,
  group_value = NULL
) {
  # If group_col is provided
  if (!is.null(group_col)) {
    res <- mod_gof

    # Keep only selected groups (if provided)
    if (!is.null(group_value)) {
      res <- res |>
        dplyr::filter(.data[[group_col]] %in% group_value)
    }

    res <- res |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::slice_min(order_by = .data[[rank_col]], n = n_best) |>
      dplyr::summarise(
        models = list(stats::setNames(.data[[effect_col]], .data[[id_col]])),
        .groups = "drop"
      )

    # Convert tibble to named list
    res <- setNames(res$models, res[[group_col]])
  } else {
    # No grouping: just get top n_best effects
    res <- mod_gof |>
      dplyr::slice_min(order_by = .data[[rank_col]], n = n_best) |>
      dplyr::pull(.data[[effect_col]])
  }

  return(res)
}


#' Generate all valid bivariable or multivariable covariate combinations
#'
#' Creates all possible combinations of covariates. It selects a covariate from
#' each list element, and retains only combinations that include the minimum
#' number of covariates. An additional column concatenates the selected
#' covariates into a single model-effect string, and a sequential model
#' identifier is assigned.
#'
#' @param covariate_list A named list of character vectors. Each list element
#'   can represent a covariate block (e.g. spatial, temporal, seasonal effects),
#'   and its values are covariate terms that can be combined.
#' @param min_var Integer. Minimum number of covariates in each
#'   required in a combination. Defaults to 2, which removes combinations
#'   containing fewer than two covariates. Set to 1 to allow univariable
#'   models, 0 to allow the empty model, or \code{NULL} to disable filtering
#'   entirely.
#' @return A tibble where each row represents a valid covariate combination.
#'   The tibble includes:
#'   \itemize{
#'     \item one column per covariate block containing the selected term
#'     \item \code{effects}: a character string of combined covariates,
#'     \item \code{model_id}: a unique model identifier (e.g. \code{mod01}).
#'   }
#'
#' @details
#' For each covariate block, an empty string is added internally to represent
#' exclusion of that block. All combinations are generated using
#' \code{tidyr::crossing()}, after which combinations containing fewer than two
#' non-empty covariates are removed.
#'
#' The \code{effects} column is constructed by concatenating covariate
#' terms row-wise, separated by \code{" + "}. This column can then be appended
#' to selected random effects and outcome to construct a full model formula.
#' @noRd
#'
.generate_combinations <- function(covariate_list, min_var = 1) {
  # Automatically add blank option to each block
  covariate_list <- lapply(covariate_list, function(x) c(x, ""))

  # Generate all combinations
  covs <- tidyr::crossing(!!!covariate_list)

  # Optionally filter by minimum number of covariates
  if (!is.null(min_var)) {
    covs <- covs |>
      dplyr::filter(
        rowSums(dplyr::across(everything(), ~ .x != "")) >= min_var
      )
  }

  # Add effects column: paste non-blank values row-wise
  covs <- covs |>
    dplyr::rowwise() |>
    dplyr::mutate(
      effects = {
        vals <- c(dplyr::across(dplyr::everything()))
        paste(vals[vals != ""], collapse = " + ")
      }
    ) |>
    dplyr::ungroup()

  covs <- covs |>
    mutate(model_id = paste0("mod", sprintf("%02d", dplyr::row_number())))
}

#' Construct INLA model formulas from fixed and random effects
#'
#' Constructs a set of INLA-compatible model formulas that share a common
#' outcome variable and random-effect structure, while differing in their
#' fixed-effect specifications.
#' Optionally includes a baseline (random effect-only) model.
#'
#' The resulting formulas are converted to \code{GHRformulas} objects for
#' downstream use in model fitting.
#'
#' @param outcome Character string of the response variable name in the data
#' set.
#' @param random_effects Character string specifying random-effect terms to be
#'   appended to every model formula (e.g. spatial, temporal, or hierarchical
#'   effects).
#' @param fixed_effects Character vector of fixed-effect specifications to be
#'   included in separate model formulas.
#' @param baseline Logical; if \code{TRUE} (default), include a baseline model
#' including only intercept and random effects.
#'
#' @return An object of class \code{GHRformulas} containing all constructed
#'   model formulas.
#'
#' @details
#' For each element of \code{fixed_effects}, a model formula of the form:
#' \preformatted{
#' outcome ~ 1 + fixed_effect + random_effects
#' }
#' is generated.
#'
#' If \code{baseline = TRUE}, an additional formula of the form:
#' \preformatted{
#' outcome ~ 1 + random_effects
#' }
#' is placed in first place in the formula set.
#'
#' All formulas are then converted to a \code{GHRformulas} object using
#' \code{as_GHRformulas()}.
#'
#' @seealso
#' \code{\link{as_GHRformulas}}
#'
#' @noRd
.paste_re_rf <- function(
  outcome,
  random_effects,
  fixed_effects,
  baseline = TRUE
) {
  # For every element in the list of fixed effect covariates
  # construct a formula by pasting the outcome and random effects
  formulas <- sapply(fixed_effects, function(x) {
    paste0(outcome, " ~ 1 + ", x, " + ", random_effects)
  })

  # Append random-effect only model formula if baseline = TRUE
  if (isTRUE(baseline)) {
    baseline <- paste0(outcome, " ~ 1 + ", random_effects)
    formulas <- c(baseline, formulas)
  }

  # Return as GHRformulas object
  formulas_ghr <- as_GHRformulas(formulas)

  return(formulas_ghr)
}


#' Format and rank goodness-of-fit results for INLA model components
#'
#' Tidies a model comparison table by inferring model structure and annotating
#' covariate metadata from one or more covariate columns. The function derives
#' the number of covariates included in each model, classifies models as
#' baseline, univariable, or multivariable, extracts root covariate patterns,
#' identifies linear versus non-linear formulations, determines lag structure,
#' and optionally ranks models by goodness-of-fit within covariate groups.
#'
#' The function assumes that covariate terms are stored across one or more
#' columns (e.g. \code{covariate_1}, \code{covariate_2}, \dots), but this
#' behaviour can be customised via \code{covariate_cols}.
#'
#' @param mod_gof A data frame or tibble containing goodness-of-fit results.
#' Must include the columns specified in the `metrics` argument and one or
#' more columns describing covariate terms. Generally
#' a GHRmodel$mod_gof data.frame.
#' @param pattern A character vector of covariate name prefixes used for
#' prefix-based matching (e.g., \code{"tmin"} matches \code{"tmin"},
#' \code{"tmin.l1"}, etc.). These prefixes are used to identify the root
#' covariate of the first covariate and populate the \code{covariate} column
#' added to \code{mod_gof}, which defines covariate-level grouping.
#' @param metric A character vector specifying the goodness-of-fit metrics
#'   passed to \code{.rank_gof()} for ranking model components.
#'   Ignored if \code{rank_by_pattern = FALSE}.
#'   Common options include:
#' \itemize{
#'   \item \code{"dic"}, \code{"waic"}, \code{"lms"}, \code{"mae"},
#'   \code{"rmse"}, \code{"crps"}, \code{"rsq"}
#' }
#'
#' @param covariate_cols A tidy-select specification identifying the columns
#'   containing covariate terms. Defaults to all columns starting with
#'   \code{"covariate_"}. Can be a single column (e.g. \code{form}) or multiple
#'   columns.
#'
#' @param rank_by_pattern Logical; if \code{TRUE} (default), models are ranked
#'   within each covariate pattern group using \code{.rank_gof()}. If
#'   \code{FALSE}, no ranking is performed.
#'
#' @details
#' For each entry in \code{covariate_1}, the function:
#' \itemize{
#'   \item Identifies the model structure and number of covariates
#'   \item Identifies the root covariate of the first covariate
#' using prefix matching against \code{pattern}
#'   \item Classifies the effect by `type` as \code{"linear"},
#'    \code{"non-linear"}, or \code{"baseline"}
#'   \item Extracts lag information from terms of the form \code{.l<number>}
#'   \item Constructs a lag-adjusted covariate label
#' }
#'
#' Model components are then grouped by covariate pattern and ranked using
#' \code{.rank_gof()} according to the supplied GoF metrics.
#'
#' @return
#' A tibble containing the original GoF results augmented with:
#' \itemize{
#'   \item \code{covariate}: identified base covariate name
#'   \item \code{type}: effect type (\code{"linear"}, \code{"non-linear"},
#'   or \code{"baseline"})
#'   \item \code{lag}: extracted lag label
#' }
#' The output is grouped and ranked within covariate groups.
#'
#' @seealso \code{\link{.rank_gof}}
#' @noRd
.tidy_model_terms <- function(
  mod_gof,
  pattern,
  metric = NULL,
  covariate_cols = tidyselect::starts_with("covariate_"),
  rank_by_pattern = TRUE
) {
  # Resolve covariate columns
  covar_cols <- mod_gof |>
    dplyr::select({{ covariate_cols }}) |>
    names()

  if (length(covar_cols) == 0) {
    stop("No covariate columns selected via `covariate_cols`.", call. = FALSE)
  }

  # Annotation
  gof <- mod_gof |>
    dplyr::mutate(
      ## Model dimension (number of covariates)
      model_dimension = rowSums(
        dplyr::across(all_of(covar_cols), ~ !is.na(.x))
      ),

      ## Structure label derived from dimension
      model_structure = as.factor(case_when(
        model_dimension == 0 ~ "baseline",
        model_dimension == 1 ~ "univariable",
        TRUE ~ "multivariable"
      )),

      # First non-missing covariate across all covariate columns.
      covariate_1 = dplyr::coalesce(!!!syms(covar_cols))
    ) |>
    dplyr::mutate(
      ## Root covariate pattern
      covariate = dplyr::case_when(
        model_dimension == 0 ~ "baseline",
        TRUE ~ sapply(
          covariate_1,
          function(x) {
            # checks whether .x starts with each pattern
            # (not preceded by a name character, literal, followed by ".")
            matches <- stringr::str_detect(
              x,
              paste0("(^|[^[:alnum:]_.])", stringr::str_escape(pattern), "\\.")
            )
            # converts TRUE/FALSE into indices
            idx <- which(matches)[1]
            # If no patterns matched, idx (index) will be NA
            # Use the index to extract covariate name from pattern
            if (is.na(idx)) NA_character_ else pattern[idx]
          }
        )
      ),

      # Linear vs non-linear
      type = dplyr::case_when(
        model_dimension == 0 ~ "baseline",
        stringr::str_detect(covariate_1, "^f\\(") ~ paste0(
          "Non-linear ",
          as.character(stringr::str_extract(covariate_1, "(?<=n=)\\d+"))
        ),
        stringr::str_detect(covariate_1, "nl_q") ~ paste0(
          "Non-linear ",
          as.character(stringr::str_extract(covariate_1, "(?<=nl_q)\\d+"))
        ),
        TRUE ~ "Linear"
      ),

      # Rolling time
      roll = case_when(
        model_dimension == 0 ~ "baseline",
        stringr::str_detect(covariate_1, "rmean") ~
          paste0(
            "Mean ",
            as.character(stringr::str_extract(covariate_1, "(?<=rmean)\\d+"))
          ),
        stringr::str_detect(covariate_1, "rsum") ~
          paste0(
            "Sum ",
            as.character(stringr::str_extract(covariate_1, "(?<=rsum)\\d+"))
          ),
        TRUE ~ "No roll"
      ),
      ,

      # Lag structure
      lag = case_when(
        model_dimension == 0 ~ "baseline",
        stringr::str_detect(covariate_1, "\\.l\\d+") ~
          stringr::str_extract(covariate_1, "(?<=.l)\\d+"),
        TRUE ~ "0"
      )
    )

  # Optional ranking by covariate pattern
  # if FALSE, do not rank at all
  if (rank_by_pattern) {
    gof <- gof |>
      group_by(.data$covariate) |>
      group_modify(~ .rank_gof(.x, metric = metric)) |>
      ungroup()
  }

  gof
}

#' Build Univariate Random Effects Formulas
#'
#' This function generates a list of univariate random effects formulas and a
#' corresponding data.frame of model IDs and formula strings, suitable for
#' use in INLA or similar modeling frameworks.
#'
#' @param re_spatial Character specifying the spatial random effect models to
#' include. Options include "bym2" (default) and NULL (no spatial RE). If not
#' NULL, an object `g` with the graph of the spatial areas must be available
#' in the working environment and the spatial identifier `spatial_id` must be
#' present in the data.
#' @param re_seasonal Character vector specifying the seasonal random effect
#' models to include. Options include "rw1", "rw2" and NULL (no seasonal RE).
#' If not NULL, the seasonal identifier `seasonal_id` must be present in the
#' data.
#' @param re_interannual Character vector specifying the interannual random
#' effect models to include. Options include "iid", "rw1" and "rw2" and NULL
#' (no interannual RE). If not NULL, the interannual identifier `year_id`
#' must be present in the data.
#' @param prior Named list of prior specifications used when constructing
#' random effect formulas.
#' @param include_baseline Logical, default TRUE. If TRUE, includes a baseline
#' empty random effect (`re_baseline`) in the output.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{effects}{Named list of formulas with model IDs as names.}
#'   \item{effects_df}{Data.frame of model IDs corresponding formula strings.}
#'   \item{prior}{The input priors list, unchanged.}
#' }
#'
#' @examples
#' priors <- list(
#'   precision.prior1 = list(
#'     prec = list(prior = "pc.prec", param = c(0.5, 0.01))
#'   ),
#'   precision.prior2 = list(
#'     prec = list(prior = "pc.prec", param = c(0.25, 0.01))
#'   )
#' )
#'
#' re_list <- .build_univariable_re(prior = priors)
#' re_list$effects_df
#'
#' @noRd
.build_univariable_re <- function(
  re_spatial,
  re_seasonal,
  re_interannual,
  prior,
  baseline = TRUE
) {
  # Build template
  template <- list()
  # Spatial
  if (!is.null(re_spatial)) {
    template$spatial <- list()
    if ("bym2" %in% re_spatial) {
      template$spatial$bym2 <- "f(spatial_id, model='bym2', graph=g, scale.model=TRUE, constr=TRUE, hyper={prior})"
    }
  }
  # Seasonal
  if (!is.null(re_seasonal)) {
    template$seasonal <- list()
    if ("rw1" %in% re_seasonal) {
      template$seasonal$rw1 <- "f(seasonal_id, model='rw1', cyclic=TRUE, scale.model=TRUE, hyper={prior}, constr=TRUE)"
    }
    if ("rw2" %in% re_seasonal) {
      template$seasonal$rw2 <- "f(seasonal_id, model='rw2', cyclic=TRUE, scale.model=TRUE, hyper={prior}, constr=TRUE)"
    }
  }
  # Interannual
  if (!is.null(re_interannual)) {
    template$interannual <- list()
    if ("iid" %in% re_interannual) {
      template$interannual$iid <- "f(year_id, model='iid', hyper={prior}, constr=TRUE)"
    }
    if ("rw1" %in% re_interannual) {
      template$interannual$rw1 <- "f(year_id, model='rw1', hyper={prior}, constr=TRUE)"
    }
    if ("rw2" %in% re_interannual) {
      template$interannual$rw2 <- "f(year_id, model='rw2', hyper={prior}, constr=TRUE)"
    }
  }
  # Baseline
  re_list <- list()
  if (isTRUE(baseline)) {
    re_list[["re_baseline"]] <- ""
  }
  counter <- list()

  for (effect_name in names(template)) {
    effect <- template[[effect_name]]
    counter[[effect_name]] <- 1

    for (spec in effect) {
      for (p in prior) {
        # Specify prior
        formula <- gsub("\\{prior\\}", p, spec)

        # Specify model ID
        model_id <- sprintf(
          "re_%s%02d",
          effect_name,
          counter[[effect_name]]
        )

        re_list[[model_id]] <- formula
        counter[[effect_name]] <- counter[[effect_name]] + 1
      }
    }
  }

  re_df <- data.frame(
    model_id = names(re_list),
    effect = unlist(re_list),
    stringsAsFactors = FALSE
  )

  list(
    effect = re_list,
    effects_df = re_df
  )
}


#' Check inputs in select_fe function
#'
#' This function check that the inputs of the arguments `roll_mean`, `roll_sum`,
#' `lag` and `nl` in `select_fe` are valid.
#'
#' @param argobj Argument to be checked.
#' @param covariates Character vector specifying the covariates inherited from
#' select_fe.
#' @param basevalue Minimum values for validation.
#'
#' @return The potentially transformed input argument (if cov = "all").
#'
#' @examples
#' covs <- c("tas", "pdsi")
#' roll_mean <- list(tas = c(1, 6), pdsi = 3)
#' .check_selectfe_input(roll_mean, covs, 1)
#'
#' @noRd
.check_selectfe_input <- function(argobj, covariates, basevalue) {
  argname <- as.character(substitute(argobj))
  if (!is.null(argobj)) {
    if (!is.list(argobj)) {
      stop(paste0("'", argname, "' must be NULL or a list."))
    }
    if (!all(names(argobj) %in% covariates)) {
      stop(paste0("All list names in '", argname, "' must be in covariates"))
    }
    if (!all(unlist(argobj) %% 1 == 0) || !all(unlist(argobj) >= basevalue)) {
      stop(paste0(
        argname,
        "' must all be a vector of integers larger than ",
        basevalue
      ))
    }
  }

  return(argobj)
}


#' Rolling statistic computed within units
#'
#' Adds a right-aligned rolling statistic of a variable, computed separately
#' for each unit so that windows never cross unit boundaries. Incomplete
#' windows at the start of each series (and series shorter than the window)
#' are \code{NA}. Assumes the data are sorted by time within unit.
#'
#' @param data A data.frame.
#' @param var Character. Name of the variable to roll.
#' @param newvar Character. Name of the column to create.
#' @param width Integer window width.
#' @param fun Function applied to each window (e.g. \code{mean}, \code{sum}).
#' @param unit Character name of the unit column, or \code{NULL} for a single
#' series.
#'
#' @return \code{data} as a data.frame with the new column added.
#' @noRd
.roll_by_unit <- function(data, var, newvar, width, fun, unit = NULL) {
  roll_one <- function(v) {
    if (length(v) < width) {
      return(rep(NA_real_, length(v)))
    }
    zoo::rollapplyr(v, width, fun, fill = NA)
  }
  data |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(unit))) |>
    dplyr::mutate(!!newvar := roll_one(.data[[var]])) |>
    dplyr::ungroup() |>
    as.data.frame()
}
