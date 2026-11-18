#' @title Random-effects model selection
#'
#' @description
#'
#' Performs a two-stage random effect model selection:
#' - Best configuration: Fits, ranks and select univariable random effect
#' models by effect type (spatial, seasonal, interannual) according to
#' goodness of fit metrics.
#' - Best combination: Generates, fits, and ranks multivariable
#' combinations of the selected univariable random effects.
#'
#' The function returns all intermediate and final results in a structured
#' output list, including generated formulas, goodness-of-fit (GoF) metrics,
#' ranked models, and selected model combinations. See details for requirements.
#'
#' WARNING: This function is experimental. Model selection should combine
#' goodness-of-fit and cross-validation metrics, which will be included in
#' future versions of the functions.
#'
#' @param data A data.frame containing the outcome and covariates used for
#' model fitting. Should contain columns named "spatial_id", "seasonal_id" and
#' "year_id" if spatial, seasonal, or interannual effects are considered.
#' @param outcome Character specifying the outcome variable name.
#' @param prior Character vector specifying the prior specifications to try
#' for each random effect. A prior with the same name must be present in the
#' working environment (see example).
#' @param time Character specifying the name of the column of `data` of class
#' 'Date' that contains the dates of the observations. Regular daily, weekly,
#' or monthly time series are allowed.
#' @param unit Character specifying the name of the column of `data` that
#' contains the spatial unit identifiers for cases where several time series are
#' available. Set to NULL for a single time series analysis.
#' @param re_spatial Character specifying the spatial random effect models to
#' include. Options include "bym2" (default) and NULL (no spatial RE). If not
#' NULL, an object `g` with the graph of the spatial units must be available
#' in the working environment and the spatial identifier `spatial_id` must be
#' present in the data (type integer). If `unit` is NULL, it will be forced
#' to be NULL too (no spatial component).
#' @param re_seasonal Character vector specifying the seasonal random effect
#' models to include. Options include "rw1", "rw2" and NULL (no seasonal RE).
#' If not NULL, the seasonal identifier `seasonal_id` must be present in the
#' data (type integer).
#' @param re_interannual Character vector specifying the interannual random
#' effect models to include. Options include "iid", "rw1" and "rw2" and NULL
#' (no interannual RE). If not NULL, the interannual identifier `year_id` must
#' be present in the data (type integer).
#' @param family Character string specifying the likelihood family
#' (default is \code{"nbinomial"}).
#' @param offset_var A character string specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param metric Optional character string specifying the ranking metric
#' used in \code{.rank_gof()}. Options include 'waic', 'lms' (the two defaults),
#' 'dic', 'rmse' and 'mae'. See [fit_models] for more details on the metrics.
#' @param n_select_uni Integer. Number of best-fitting univariable models to
#' select per random effect group.
#' @param n_select_multi Integer. Number of best-fitting multivariable models to
#' display as final candidates in the report.
#' @param nthreads An integer specifying the maximum number of threads for
#' parallel computation. Default is \code{1}.
#'
#' @return
#' A named list of class GHRselect_re containing:
#' \itemize{
#'   \item \code{params}: Selection of input parameters used in the reporting
#'   of the results.
#'   \item \code{data}: Data used for all the model runs.
#'   \item \code{re_uni_mod}: Univariable random effect models.
#'   \item \code{re_uni_gof}: GoF results for univariable random effect models.
#'   \item \code{re_uni_selected}: Selected univariable random effects.
#'   \item \code{re_multi_mod}: Multivariable random effect models.
#'   \item \code{re_multi_gof}: GoF results for multivariable random effect
#'   models.
#'   \item \code{re_multi_selected}: Selected multivariable random effects.
#'   \item \code{best}: Best-fitting multivariable model formula.
#' }
#'
#' @seealso [select_report] [select_fe]
#'
#' @examples
#' \dontrun{
#' # Read data
#' library(sf)
#' data(dengue_MS)
#' dengue_MS <- dengue_MS[complete.cases(dengue_MS), ]
#'
#' # Compute graph
#' data(map_MS)
#' nb <- spdep::poly2nb(map_MS)
#' g <- spdep::nb2mat(nb, style = "B")
#'
#' # Prepare indices
#' dengue_MS$spatial_id <- as.numeric(as.factor(dengue_MS$micro_code))
#' dengue_MS$seasonal_id <- dengue_MS$month
#' dengue_MS$year_id <- dengue_MS$year
#'
#' # Prepare precision priors
#' prec1 <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
#' prec2 <- list(prec = list(prior = "pc.prec", param = c(0.25, 0.01)))
#'
#' # Run RE model selection
#' dengue_re <- select_re(
#'   data = dengue_MS,
#'   outcome = "dengue_cases",
#'   prior = c("prec1", "prec2"),
#'   offset_var = "pop100000",
#'   time = "date",
#'   unit = "micro_name"
#' )
#'
#' # Generate report
#' select_report(dengue_re, file.path(tempdir(), "random_effects.html"))
#' }
#' @export

select_re <- function(
  data,
  outcome,
  prior,
  time,
  unit = NULL,
  re_spatial = "bym2",
  re_seasonal = c("rw1", "rw2"),
  re_interannual = c("iid", "rw1", "rw2"),
  family = "nbinomial",
  offset_var = NULL,
  metric = c("waic", "lms"),
  n_select_uni = 1,
  n_select_multi = 3,
  nthreads = 1
) {
  # 0. Checks ----

  # Required
  rlang::check_required(data)
  rlang::check_required(outcome)
  rlang::check_required(prior)
  rlang::check_required(time)
  rlang::check_required(unit)

  # Outcome check
  if (is.null(data[[outcome]])) {
    stop("'outcome' not found in the data")
  }

  # Offset check
  if (!is.null(offset_var)) {
    if (is.null(data[[offset_var]])) {
      stop("'offset_var' not found in the data")
    }
  }

  # time and unit
  if (is.null(data[[time]])) {
    stop("No column of the data matches the 'time' argument")
  } else if (any(is.na(data[[time]]))) {
    stop("No missings in the 'time' columns are permitted.")
  }
  if (!is.null(unit) && is.null(data[[unit]])) {
    stop("No column of the data matches the 'unit' argument")
  }
  if (!is.null(unit)) {
    if (any(is.na(data[[unit]]))) {
      stop("No missings in the 'unit' columns are permitted.")
    }
    GHRexplore::check_consecutive(data, time, unit)
  } else {
    GHRexplore::check_consecutive(data, time)
  }

  # Identifier and model check
  if (is.null(unit) && !is.null(re_spatial)) {
    re_spatial <- NULL
    message(
      "Setting 're_spatial' to NULL since there's no ",
      "spatial component in the data."
    )
  }
  if (!is.null(re_spatial)) {
    if (is.null(data[["spatial_id"]])) {
      stop("'spatial_id' not found in the data")
    }
    if (!exists("g")) {
      stop("Spatial graph 'g' not found in the environment")
    } # Check class?
    if (re_spatial != "bym2") {
      stop("Invalid spatial model")
    }
    if (is.null(unit)) {
      stop("'unit' cannot be NULL if a spatial_id is chosen")
    }
  }
  if (!is.null(re_seasonal)) {
    if (is.null(data[["seasonal_id"]])) {
      stop("'seasonal_id' not found in the data")
    }
    if (!all(re_seasonal %in% c("rw1", "rw2"))) {
      stop("Invalid seasonal model")
    }
  }
  if (!is.null(re_interannual)) {
    if (is.null(data[["year_id"]])) {
      stop("'year_id' not found in the data")
    }
    if (!all(re_interannual %in% c("iid", "rw1", "rw2"))) {
      stop("Invalid interannual model")
    }
  }
  if (is.null(re_spatial) && is.null(re_seasonal) && is.null(re_interannual)) {
    stop("At least one of the random effects must be non-null.")
  }

  # Prior check
  for (p in prior) {
    if (!exists(p)) {
      stop("Prior ", p, " not available in the environment.")
    }
  }

  # Metric check
  if (!all(metric %in% c("waic", "lms", "dic", "rmse", "mae"))) {
    stop("Invalid chosen metric.")
  }

  # 1. Pre-processing ----

  # Data subsetting
  data_cols <- c(
    outcome,
    offset_var,
    unit,
    time,
    "spatial_id",
    "seasonal_id",
    "year_id"
  )
  data_mod <- data[, data_cols[data_cols %in% names(data)]]
  if (any(sapply(data_mod[, -1], function(x) (any(is.na(x)))))) {
    stop(paste0(
      "No missings in 'offset_var', 'unit', 'time', 'spatial_id',",
      "'seasonal_id', 'year_id' are allowed."
    ))
  }

  # Output placeholder and params
  out_list <- list()
  out_list$params <- list(
    outcome = outcome,
    unit = unit,
    time = time,
    metric = metric,
    family = family,
    offset_var = offset_var
  )
  out_list$data <- data_mod

  # 2. Univariable models ----
  message("Fitting univariable models...")

  # Generate univariable random effect formulas
  re_effects <- .build_univariable_re(
    re_spatial = re_spatial,
    re_seasonal = re_seasonal,
    re_interannual = re_interannual,
    prior = prior,
    baseline = TRUE
  )

  # Fit univariable random effect formulas
  re_uni_models <- .model_batch(
    outcome = outcome,
    effect = re_effects$effect,
    data = data_mod,
    family = family,
    offset_var = offset_var,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = nthreads
  )
  out_list$re_uni_mod <- .stack_models_safe(re_uni_models[["mod_list"]])
  re_uni_gof <- do.call(rbind, re_uni_models[["gof_list"]])
  re_uni_gof$effect <- NULL

  # Add models and effect types to the individual effects
  re_uni_df <- re_effects$effects_df |>
    dplyr::mutate(
      model = dplyr::case_when(
        grepl("bym2", effect, fixed = TRUE) ~ "bym2",
        grepl("rw1", effect, fixed = TRUE) ~ "rw1",
        grepl("rw2", effect, fixed = TRUE) ~ "rw2",
        grepl("iid", effect, fixed = TRUE) ~ "iid",
        TRUE ~ NA
      )
    ) |>
    dplyr::mutate(
      prior = stringr::str_extract(.data$effect, "(?<=hyper=)[^,)]+")
    ) |>
    dplyr::mutate(
      effect_group = dplyr::case_when(
        grepl("_spatial", model_id, fixed = TRUE) ~ "spatial",
        grepl("_interannual", model_id, fixed = TRUE) ~ "interannual",
        grepl("_seasonal", model_id, fixed = TRUE) ~ "seasonal",
        T ~ "baseline"
      ),
    )

  # Add to gof
  re_uni_gof <- dplyr::left_join(re_uni_gof, re_uni_df, by = "model_id")

  # return ranked gof data frame for all univariable random effects
  re_uni_gof_rank <- re_uni_gof |>
    split(re_uni_gof$effect_group) |>
    lapply(.rank_gof, metric = metric)
  re_uni_gof_rank <- do.call(rbind, re_uni_gof_rank)
  row.names(re_uni_gof_rank) <- NULL

  # Store the object
  out_list$re_uni_gof <- re_uni_gof_rank

  # select the average best-fitting models for each effect category
  # (the baseline model has no effect to combine, so it is excluded here;
  # it remains available for reference in out_list$re_uni_gof)
  re_uni_selected <- .select_effects(
    mod_gof = dplyr::filter(re_uni_gof_rank, .data$effect_group != "baseline"),
    n_best = n_select_uni,
    rank_col = "final_rank",
    id_col = "model_id",
    group_col = "effect_group",
    effect_col = "effect",
  )
  out_list$re_uni_selected <- re_uni_selected

  # 3. Multivariable models ----
  message("Fitting multivariable models...")

  # Generate combinations of the selected univariable random effects
  re_combinations <- .generate_combinations(re_uni_selected)

  # use the re_uni_df data frame to include the original univariable model_id
  # corresponding to each random effect
  re_uni_df <- re_uni_df |>
    dplyr::filter(.data$effect != "") |>
    dplyr::select(model_id, "effect")

  # identify which random effect group names are being used
  re_names <- setdiff(
    names(re_combinations),
    c("effects", "model_id")
  )

  # format the combinations
  re_combinations <- re_combinations |>
    # rename model ids
    mutate(model_id = paste0("mod", sprintf("%02d", dplyr::row_number())))

  # loop over each random-effect name
  for (re in re_names) {
    # for each random-effect name, join its corresponding univariable model_id
    re_combinations <- re_combinations |>
      dplyr::left_join(
        # build a lookup table from univariable RE models
        re_uni_df |>
          # select and rename columns dynamically:
          # - create a new column named "<re>_id" (the univariable model_id)
          # - rename `effect` to the current random-effect name
          dplyr::select(
            !!paste0(re, "_id") := model_id,
            !!re := "effect"
          ),
        # join by the effect formula string (e.g. monthly, spatial, yearly)
        by = re
      )
  }

  # Generate named vector of the selected random effect combinations
  re_multi_formulas <- rlang::set_names(
    re_combinations$effects,
    re_combinations$model_id
  )

  # Fit models with selected random effect combinations
  re_multi_models <- .model_batch(
    outcome = outcome,
    effect = re_multi_formulas,
    data = data_mod,
    family = family,
    offset_var = offset_var,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = nthreads
  )
  out_list$re_multi_mod <- .stack_models_safe(re_multi_models[["mod_list"]])
  re_multi_gof <- do.call(rbind, re_multi_models[["gof_list"]])
  # dropped here (rather than kept and renamed) since re_combinations already
  # carries the same formula string in its 'effects' column
  re_multi_gof$effect <- NULL

  # Rank multivariable random effects models
  # return ranked gof data frame for all multivariable random effects
  re_multi_gof_rank <- .rank_gof(
    mod_gof = re_multi_gof,
    metric = metric
  )

  # Assign random effect combinations to each model
  re_multi_gof_rank <- re_multi_gof_rank |>
    # add the random effect combinations that each model corresponds to
    left_join(re_combinations, by = "model_id")

  # generate a gof dataframe with the re combinations
  out_list$re_multi_gof <- re_multi_gof_rank

  # Keep the n_select_multi and best one
  mod_ordered <- re_multi_gof_rank[order(re_multi_gof_rank$final_rank), ]
  bestnamek <- mod_ordered[["model_id"]][
    seq_len(min(nrow(mod_ordered), n_select_multi))
  ]
  bestmodk <- mod_ordered[["effects"]][
    seq_len(min(nrow(mod_ordered), n_select_multi))
  ]
  names(bestmodk) <- bestnamek
  out_list$re_multi_selected <- bestmodk
  out_list$best <- bestmodk[1]

  class(out_list) <- append("GHRselect_re", class(out_list))
  return(out_list)
}
