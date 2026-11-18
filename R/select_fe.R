#' @title Fixed-effects model selection
#'
#' @description
#' Performs a two-stage fixed effect model selection:
#' - Best configuration: Fits, ranks and selects univariable fixed effect
#' models by covariate (rolling averages, lags, non-linear) according to
#' goodness of fit metrics.
#' - Best combination: Generates, fits, and ranks multivariable
#' combinations of the selected univariable fixed effects.
#'
#' The function returns all intermediate and final results in a structured
#' output list, including generated formulas, goodness-of-fit (GoF) metrics,
#' ranked models, and selected model combinations.
#'
#' WARNING: This function is experimental. Model selection should combine
#' goodness-of-fit and cross-validation metrics, which will be included in
#' future versions of the functions.
#'
#' @param data A data.frame containing the outcome and covariates used for
#' model fitting. Should contain columns named "spatial_id", "seasonal_id" and
#' "year_id" if spatial, seasonal, or interannual effects are considered.
#' @param outcome Character specifying the outcome variable name.
#' @param covariates Names of the covariates in the data to consider.
#' @param time Character specifying the name of the column of `data` that
#' contains the dates of the observations. Regular daily, weekly, or monthly
#' time series are allowed.
#' @param unit Character specifying the name of the column of `data` that
#' contains the spatial unit identifiers for cases where several time series are
#' available. Set to NULL for a single time series analysis.
#' @param re_mod Either the output of \code{select_re} (i.e. a list with
#' a re_multi_gof component), or alternatively a single character with a random
#' effects formula.
#' @param re_id Only used if \code{re_mod} is the output of
#' \code{select_re}. ID of the selected random effects models to use as in
#' \code{select_re$re_multi_gof$model_id}. If NULL (default), it will use the
#' best model according to the GoF statistics.
#' @param roll_mean A named list specifying the variables in `covariates`
#' to compute rolling means for with averaging times `t` >= 1. Note that t=1
#' is equivalent to the non-transformed covariate denoted as 'noroll'.
#' See example to see format. If a variable in `covariates` is included in
#' neither `roll_mean` nor `roll_sum` (default), only the non-transformed
#' covariate (i.e. 'noroll') is included.
#' @param roll_sum Same as `roll_mean` but with a sum. Defaults to NULL. The
#' rolling windows requested in `roll_mean` and `roll_sum` are combined.
#' @param lag A named list specifying the variables in `covariates` to compute
#' lags with lag times `t` >= 0. See example to see format.
#' Lag times are applied after rolling statistics. Note that t=0
#' (i.e. 'lag0') is equivalent to the non-transformed covariate.
#'  If a variable in `covariates` is not included in the list (default), only
#' the non-transformed covariate (i.e. 'lag0') is included.
#' @param nl A named list specifying the variables in `covariates` to test in
#' non-linear form using n >= 3 cut points. See example to see format.
#' Non-linear effects are applied after rolling statistics and lags. If a
#' variable in `covariates` is not included in the list, only the linear
#' effect will be computed. Set to NULL (default) to only include linear
#' effects.
#' @param family Character string specifying the likelihood family
#' (default is \code{"nbinomial"}). See [fit_models] to check the available
#' families.
#' @param offset_var A character specifying the name of the offset
#' variable in \code{data}. If \code{NULL} (default), no offset is applied.
#' Internally, \code{offset_var} is transformed according to the link function
#' of the corresponding family: identity for \code{"gaussian"} and log
#' for the rest of families.
#' @param metric Character vector specifying the ranking metric used
#' to rank models. Options include 'waic', 'lms', 're_meanchange' (defaults),
#' 'dic', 'rmse' and 'mae'. See [fit_models] for more details on the metrics.
#' @param n_select_uni Integer. Number of best-fitting univariable models to
#' select per covariate group.
#' @param n_select_multi Integer. Number of best-fitting multivariable models to
#' display as final candidates. Note that only combinations with at least
#' length(covariates) - 1) are considered.
#' @param standardize Character. Specifies which variables should be
#' standardised, which can help model convergence. Standardization is applied
#' after rolling statistics and before lagging, so that the mean of all
#' non-lagged variables is equal to 0 and standard deviation is 1 (these may
#' vary slightly for lagged versions due to dropped rows).
#' Set to NULL for no standardization, set to 'all' (default) to apply to
#' all covariates or use specific covariate names.
#' @param nthreads An integer specifying the maximum number of threads for
#' parallel computation. Default is \code{1}.
#'
#' @return
#' A list of class GHRselect_fe containing:
#' \itemize{
#'   \item \code{params}: Selection of input parameters used in the reporting
#'   of the results.
#'   \item \code{data}: Data used for all the model runs after transformations.
#'   \item \code{fe_uni_mod}: Univariable fixed effect models.
#'   \item \code{fe_uni_gof}: GoF results for univariable fixed effect models.
#'   \item \code{fe_uni_selected}: Selected univariable fixed effect models.
#'   \item \code{fe_multi_gof}: GoF results for multivariable fixed effect
#'   models.
#'   \item \code{fe_multi_mod}: Multivariable fixed effect models.
#'   \item \code{fe_multi_selected}: Selected multivariable fixed effects.
#'   \item \code{best}: Best-fitting multivariable model formula.
#' }
#'
#' @seealso [select_report] [select_re]
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
#'
#' # Run FE model selection
#' dengue_fe <- select_fe(
#'   data = dengue_MS,
#'   outcome = "dengue_cases",
#'   covariates = c("tmax", "pdsi"),
#'   roll_mean = list(tmax = c(1, 6), pdsi = c(1, 6)),
#'   lag = list(tmax = c(1, 3), pdsi = c(1, 3)),
#'   nl = list(tmax = c(4, 8), pdsi = 8),
#'   offset_var = "pop100000",
#'   time = "date",
#'   unit = "micro_name",
#'   re_mod = dengue_re
#' )
#'
#' # Generate report
#' select_report(dengue_fe, file.path(tempdir(), "fixed_effects.html"))
#' }
#' @export

select_fe <- function(
  data,
  outcome,
  covariates,
  time,
  unit = NULL,
  re_mod,
  re_id = NULL,
  roll_mean = NULL,
  roll_sum = NULL,
  lag = NULL,
  nl = NULL,
  family = "nbinomial",
  offset_var = NULL,
  metric = c("waic", "lms", "re_meanchange"),
  n_select_uni = 1,
  n_select_multi = 3,
  standardize = "all",
  nthreads = 1
) {
  # 0. Checks ----

  # Required
  rlang::check_required(data)
  rlang::check_required(outcome)
  rlang::check_required(covariates)
  rlang::check_required(time)
  rlang::check_required(unit)
  rlang::check_required(re_mod)

  # Relevant cols
  keep_cols <- c()

  # Outcome and covariate check
  if (is.null(data[[outcome]])) {
    stop("'outcome' not found in the data")
  }
  for (cov in covariates) {
    if (is.null(data[[cov]])) {
      stop(paste0("Covariate ", cov, " not found in the data"))
    }
  }

  # Offset check
  if (!is.null(offset_var)) {
    if (is.null(data[[offset_var]])) {
      stop("'offset_var' not found in the data")
    }
    keep_cols <- c(keep_cols, offset_var)
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
    keep_cols <- c(keep_cols, unit)
  } else {
    GHRexplore::check_consecutive(data, time)
  }

  # Random effect checks: character or GHRselect_re
  if (is.character(re_mod) || inherits(re_mod, "GHRselect_re")) {
    if (is.list(re_mod)) {
      re_allid <- re_mod[["re_multi_gof"]][["model_id"]]
      re_alleffects <- re_mod[["re_multi_gof"]][["effects"]]
      if (!is.null(re_id)) {
        if (!is.character(re_id) || length(re_id) != 1) {
          stop("'re_id' must be a single character model ID.")
        }
        if (!re_id %in% re_allid) {
          stop("'re_id' not found in 're_mod'. Please change to a valid ID.")
        }
        re_mod <- re_alleffects[which(re_id == re_allid)[1]]
      } else {
        re_mod <- re_mod$best
      }
    }
  } else {
    stop(
      "'re_mod' must be either a character formula ",
      "or the output of `select_re`"
    )
  }
  if (!is.character(re_mod) || length(re_mod) != 1 || is.na(re_mod)) {
    stop("The random effects formula in 're_mod' must be a single string.")
  }

  # Random effect checks: identifiers and priors
  if (grepl("spatial_id", re_mod)) {
    if (is.null(data[["spatial_id"]])) {
      stop("'spatial_id' not found in the data")
    }
    if (!exists("g")) {
      stop("Spatial graph 'g' not found in the environment")
    }
    keep_cols <- c(keep_cols, "spatial_id")
  }
  if (grepl("seasonal_id", re_mod)) {
    if (is.null(data[["seasonal_id"]])) {
      stop("'seasonal_id' not found in the data")
    }
    keep_cols <- c(keep_cols, "seasonal_id")
  }
  if (grepl("year_id", re_mod)) {
    if (is.null(data[["year_id"]])) {
      stop("'year_id' not found in the data")
    }
    keep_cols <- c(keep_cols, "year_id")
  }

  # Fixed effects checks: roll_mean, roll_sum, nl
  roll_mean <- .check_selectfe_input(roll_mean, covariates, 1)
  roll_sum <- .check_selectfe_input(roll_sum, covariates, 1)
  nl <- .check_selectfe_input(nl, covariates, 3)
  lag <- .check_selectfe_input(lag, covariates, 0)

  # Metric check
  if (
    !all(
      metric %in% c("waic", "lms", "dic", "rmse", "mae", "re_meanchange")
    )
  ) {
    stop("Invalid chosen metric.")
  }

  # Standardization
  if (is.null(standardize)) {
    standardize <- ""
  } else if (!is.character(standardize)) {
    stop("'standardize' must be character.")
  } else if (any(standardize %in% "all")) {
    standardize <- covariates
  } else if (any(!standardize %in% c(covariates))) {
    stop("All variables in 'standardize' must be in 'covariates'.")
  }
  if (all(standardize == "")) {
    message("No covariates have been standardized.")
  } else {
    message(
      "Standardized covariates: ",
      paste(standardize, collapse = ", "),
      "."
    )
  }

  # 1. Pre-processing ----

  # Data subsetting
  data_cols <- unique(c(
    outcome,
    covariates,
    time,
    keep_cols
  ))
  data_mod <- as.data.frame(data[, names(data) %in% data_cols])
  if (
    any(sapply(data_mod[, names(data_mod) != outcome], function(x) {
      (any(is.na(x)))
    }))
  ) {
    stop(paste0(
      "No missings in time, ",
      paste(keep_cols, collapse = ", "),
      " or any covariate are allowed."
    ))
  }

  # Sort by unit and time
  if (is.null(unit)) {
    data_mod <- data_mod[order(data_mod[[time]]), , drop = FALSE]
  } else {
    data_mod <- data_mod[
      order(data_mod[[unit]], data_mod[[time]]),
      ,
      drop = FALSE
    ]
  }
  rownames(data_mod) <- NULL

  # List of covariates
  all_covs <- setNames(vector("list", length(covariates)), covariates)

  # Rolling statistics
  for (cov in covariates) {
    # Requested transformations as (statistic, window) pairs
    specs <- list()
    if (cov %in% names(roll_mean)) {
      specs <- c(specs, lapply(roll_mean[[cov]], function(t) list("mean", t)))
    }
    if (cov %in% names(roll_sum)) {
      specs <- c(specs, lapply(roll_sum[[cov]], function(t) list("sum", t)))
    }
    if (length(specs) == 0) {
      # Non-transformed
      specs <- list(list("mean", 1))
    }
    specs <- specs[
      !duplicated(lapply(specs, function(s) {
        if (s[[2]] == 1) "noroll" else s
      }))
    ]

    for (s in specs) {
      fun_name <- s[[1]]
      t <- s[[2]]
      if (t == 1) {
        newvar <- paste0(cov, ".noroll")
        data_mod[[newvar]] <- data_mod[[cov]]
      } else {
        newvar <- paste0(cov, if (fun_name == "mean") ".rmean" else ".rsum", t)
        data_mod <- .roll_by_unit(
          data_mod,
          var = cov,
          newvar = newvar,
          width = t,
          fun = if (fun_name == "mean") mean else sum,
          unit = unit
        )
      }
      all_covs[[cov]] <- c(all_covs[[cov]], newvar)
      # Standardization
      if (cov %in% standardize) {
        data_mod[[newvar]] <- data_mod[[newvar]] -
          mean(data_mod[[newvar]], na.rm = TRUE)
        data_mod[[newvar]] <- data_mod[[newvar]] /
          stats::sd(data_mod[[newvar]], na.rm = TRUE)
      }
    }
  }
  all_covs <- lapply(all_covs, unique)

  # Lags
  if (is.null(lag)) {
    lag <- setNames(as.list(rep(0, length(covariates))), covariates)
  } else {
    miscov <- setdiff(covariates, names(lag))
    if (length(miscov) > 0) {
      for (m in miscov) {
        lag[[m]] <- 0
      }
    }
  }
  all_covs_lag <- setNames(vector("list", length(covariates)), covariates)
  for (cov in names(all_covs)) {
    for (var in all_covs[[cov]]) {
      for (t in lag[[cov]]) {
        if (t == 0) {
          newvar <- paste0(var, ".l0")
          data_mod[[newvar]] <- data_mod[[var]]
          all_covs_lag[[cov]] <- c(all_covs_lag[[cov]], newvar)
        } else {
          if (is.null(unit)) {
            newvar <- paste0(var, ".l", t)
            data_mod[[newvar]] <- c(lag_cov(
              data = data_mod,
              name = var,
              time = time,
              lag = t,
              add = FALSE
            ))
          } else {
            newvar <- paste0(var, ".l", t)
            data_mod[[newvar]] <- c(lag_cov(
              data = data_mod,
              name = var,
              time = time,
              lag = t,
              group = unit,
              add = FALSE
            ))
          }
          all_covs_lag[[cov]] <- c(all_covs_lag[[cov]], newvar)
        }
      }
    }
  }
  all_covs_lag <- lapply(all_covs_lag, unique)

  # Clean data.frame
  all_covs_lag <- unname(c(unlist(all_covs_lag)))
  data_mod <- data_mod[, c(data_cols, all_covs_lag)]
  # Time points lost per unit: rolling windows (width - 1) plus lags, which
  # only combine within the same covariate
  n_lost <- max(
    0,
    vapply(
      covariates,
      function(cv) {
        max(0, roll_mean[[cv]], roll_sum[[cv]], 1) - 1 + max(0, lag[[cv]])
      },
      numeric(1)
    )
  )
  if (n_lost > 0) {
    message(paste0(
      "The first ",
      n_lost,
      " time points of each unit have been removed from the training data ",
      "due to the used rolling and lag structure."
    ))
    data_mod <- data_mod[
      stats::complete.cases(data_mod[,
        names(data_mod) != outcome,
        drop = FALSE
      ]),
    ]
  }

  # Output placeholder and params
  out_list <- list()
  out_list$params <- list(
    outcome = outcome,
    covariates = covariates,
    unit = unit,
    time = time,
    metric = metric,
    re_form = re_mod,
    family = family,
    offset_var = offset_var,
    roll_mean = roll_mean,
    roll_sum = roll_sum,
    lag = lag,
    nl = nl,
    standardize = standardize
  )
  out_list$data <- data_mod

  # Random effects message
  message("Using random effects model formula: ")
  message(re_mod)

  # 2. Univariate models ----
  message("Starting univariable analysis...")

  # Generate the univariable linear formulas
  form_lin <- cov_uni(all_covs_lag, name = all_covs_lag)
  uni_terms <- unlist(form_lin)

  # Generate the univariable non-linear formulas
  if (!is.null(nl)) {
    for (cov in names(nl)) {
      for (n in nl[[cov]]) {
        try(
          {
            form_nl <- cov_nl(all_covs_lag, pattern = paste0(cov, "."), n = n)
            uni_terms <- c(uni_terms, unlist(form_nl))
          },
          silent = TRUE
        )
      }
    }
  }
  uni_terms <- uni_terms[!duplicated(uni_terms)]

  # Model term formulas
  uni_formulas <- .paste_re_rf(
    outcome = outcome,
    random_effects = re_mod,
    fixed_effects = uni_terms,
    baseline = TRUE
  )

  # Tidy model table
  uni_df <- data.frame(form = uni_terms)
  uni_df <- .tidy_model_terms(
    mod_gof = uni_df,
    pattern = covariates,
    metric = metric,
    covariate_cols = "form",
    rank_by_pattern = FALSE
  ) |>
    dplyr::select(
      -c("covariate_1", "model_dimension", "model_structure")
    )

  # Fit models
  uni_model <- fit_models(
    data = data_mod,
    formulas = uni_formulas,
    name = "mod",
    family = family,
    offset_var = offset_var,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = nthreads
  )
  out_list$fe_uni_mod <- uni_model

  # Clean GoF
  uni_gof <- .tidy_model_terms(
    mod_gof = uni_model$mod_gof,
    pattern = covariates,
    metric = metric,
    covariate_cols = "covariate_1",
    rank_by_pattern = TRUE
  )
  uni_gof <- uni_gof |>
    dplyr::left_join(uni_df, by = c("covariate", "lag", "roll", "type")) |>
    dplyr::filter(.data$covariate != "baseline")
  out_list$fe_uni_gof <- uni_gof

  # Select univariate
  uni_fe_selected <- .select_effects(
    mod_gof = uni_gof,
    n_best = n_select_uni,
    rank_col = "final_rank",
    id_col = "model_id",
    group_col = "covariate",
    effect_col = "form"
  )
  message("The following terms have been selected in univariable analysis")
  for (cov in covariates) {
    message(paste0(cov, ": ", paste(uni_fe_selected[[cov]], collapse = ", ")))
  }
  out_list$fe_uni_selected <- uni_fe_selected

  # 3. Multivariable models ----
  message("Starting multivariable analysis...")

  # Generate combinations
  multi_df <- .generate_combinations(
    uni_fe_selected,
    min_var = max(1, length(covariates) - 1)
  ) |>
    dplyr::select(-model_id)

  # Generate list and formulas of combined covariates
  multi_list <- as.list(multi_df$effects)
  multi_form <- .paste_re_rf(
    outcome = outcome,
    random_effects = re_mod,
    fixed_effects = multi_list,
    baseline = TRUE
  )

  # Fit multivariable models
  multi_models <- fit_models(
    data = data_mod,
    formulas = multi_form,
    name = "mod",
    family = family,
    offset_var = offset_var,
    control_compute = list(config = FALSE, vcov = FALSE),
    nthreads = nthreads
  )
  out_list$fe_multi_mod <- multi_models

  # Rank and annotate univariable fixed effects models
  multi_gof <- .tidy_model_terms(
    mod_gof = multi_models$mod_gof,
    pattern = covariates,
    metric = NULL,
    rank_by_pattern = FALSE
  ) |>
    select(-c("covariate", "type", "lag", "roll")) |>
    # The baseline (random effects only) model is fitted as a reference for
    # relative metrics, but must not be ranked or selected as a candidate
    dplyr::filter(.data$model_structure != "baseline")

  # Rank multivariable random effects models
  # return ranked gof data frame for all multivariable random effects
  multi_gof_rank <- .rank_gof(
    mod_gof = multi_gof,
    metric = metric
  )

  # Assign random effect combinations to each model
  multi_df$model_id <- paste0("mod", seq_len(nrow(multi_df)) + 1)
  multi_gof_rank <- multi_gof_rank |>
    # add the random effect combinations that each model corresponds to
    dplyr::left_join(multi_df, by = "model_id")

  # generate a gof dataframe with the re combinations
  out_list$fe_multi_gof <- multi_gof_rank

  # Keep the n_select_multi and best one
  mod_ordered <- multi_gof_rank[order(multi_gof_rank$final_rank), ]
  bestnamek <- mod_ordered[["model_id"]][
    seq_len(min(nrow(mod_ordered), n_select_multi))
  ]
  bestmodk <- mod_ordered[["effects"]][
    seq_len(min(nrow(mod_ordered), n_select_multi))
  ]
  names(bestmodk) <- bestnamek
  out_list$fe_multi_selected <- bestmodk
  bestname <- mod_ordered[["model_id"]][1]
  bestmod <- mod_ordered[["effects"]][1]
  names(bestmod) <- bestname
  out_list$best <- bestmod

  class(out_list) <- append("GHRselect_fe", class(out_list))
  return(out_list)
}
