#' Fit Multiple INLA Models
#'
#' This function fits a set of \code{INLA} model formulas, provided in a \code{GHRformulas} object,
#' to a specified dataset. For each fitted model, it extracts a range of outputs, 
#' including goodness-of-fit (GoF) metrics and other model outputs (fitted values, fixed effects, random effects).
#' Results are extracted and stored in a `GHRmodels` object. 
#'
#' @param formulas A \code{GHRformulas} object containing multiple INLA model formulas.
#' @param data A data frame containing the variables used in the model formulas.
#' @param family A character string specifying the likelihood family
#'  (e.g., \code{"poisson"}, \code{"nbinomial"}, etc.).
#' @param name A character string to label each fitted model (e.g., \code{"mod"}).
#' @param offset A character string specifying the name of the offset variable in \code{data}. 
#'   If \code{NULL}, no offset is applied. Default is \code{NULL}.
#'   Internally, \code{log(offset_values)} is applied.
#' @param pb Logical; if \code{TRUE}, displays a progress bar while fitting models. Default is \code{FALSE}.
#' @param control_compute A named list controlling additional computation options:
#'   \describe{
#'     \item{\code{config}}{Logical ; if \code{TRUE}, stores the Gaussian Markov Random Field (GMRF) and enables the 
#'     computation of posterior predictive distribution (1,000 draws). Defaults to \code{FALSE}.}
#'     \item{\code{vcov}}{Logical if \code{TRUE}, returns  the variance-covariance 
#'       (correlation) matrix of fixed effects. Defaults to \code{FALSE}.}
#'   }
#' @param nthreads An integer specifying the number of threads for parallel computation. Default is \code{8}.
#'
#' @return An object of class \code{GHRmodels} containing:
#' \describe{
#'   \item{`$mod_gof`}{A data frame of model-specific goodness-of-fit metrics.}
#'   \item{`$fitted`}{A list of fitted values (one element per model). 
#'   If \code{config = TRUE}, these are derived from the posterior predictive distribution (PPD); 
#'   otherwise, they are extracted from INLA's \code{summary.fitted.values}.}
#'   \item{`$fixed`}{A list of summary tables for fixed effects (one element per model).}
#'   \item{`$random`}{A list of summary tables for random effects (one element per model).}
#'   \item{`$formulas`}{A character vector of the original model formulas used.}
#'   \item{`$re`}{A character vector specifying any random effects defined in \code{formulas}.}
#'   \item{`$outcome`}{A character string indicating the outcome variable used.}
#'   \item{`$data`}{The original data frame passed to the function.}
#' }
#' 
#' @details
#'
#' This function iterates over each formula in the \code{GHRformulas} object 
#' and fits the corresponding \code{INLA} model using the internal function 
#' \code{.fit_single_model()}. For each fitted model, it extracts the fitted values, 
#' fixed effects, and random effects summaries. Then, it calculates a series of 
#' model evaluation metrics using the \code{.gof_single_model()} internal function.
#'
#' The goodness-of-fit (GoF) metrics are organized into two categories:
#'
#' **A) Model-Specific Goodness-of-Fit Metrics**
#'
#' These are computed separately for each model:
#'
#' 1. **Deviance Information Criterion (DIC)**  
#'    \deqn{DIC = \bar{D} + p_{D}}  
#'    where \eqn{\bar{D}} is the posterior mean deviance and \eqn{p_{D}} is the
#'    effective number of parameters. Lower DIC values indicate a better model fit,
#'    balancing goodness-of-fit and model complexity.
#'
#' 2. **Watanabe-Akaike Information Criterion (WAIC)**  
#'    \deqn{WAIC = -2\left(\mathrm{lppd} - p_{\mathrm{WAIC}}\right)}  
#'    WAIC evaluates predictive accuracy and penalizes model complexity through 
#'    the log pointwise predictive density (\eqn{\mathrm{lppd}}). Lower values 
#'    imply better generalization.
#'
#' 3. **Log Mean Score (LMS)**  
#'    \deqn{LMS = \frac{1}{n} \sum_{i=1}^n \left( -\log(\mathrm{CPO}_i) \right)}  
#'    LMS assesses the average negative log-predictive density using Conditional Predictive 
#'    Ordinates (CPO). Lower LMS values indicate stronger predictive performance by 
#'    penalizing models that assign low probability to observed outcomes.
#'
#' 4. **Mean Absolute Error (MAE)**  
#'    \deqn{MAE = \frac{1}{n} \sum_{i=1}^n \left| y_i - \hat{y}_i \right|}  
#'    Measures the average absolute deviation between observed values \eqn{y_i} and 
#'    predicted values \eqn{\hat{y}_i}. Lower MAE values indicate improved fit.  
#'    If \code{config = TRUE}, MAE is computed using the full posterior predictive distribution (PPD); 
#'    otherwise, it uses point estimates from INLA's \code{summary.fitted.values}.
#'
#' 5. **Root Mean Squared Error (RMSE)**  
#'    \deqn{RMSE = \sqrt{ \frac{1}{n} \sum_{i=1}^n (y_i - \hat{y}_i)^2 }}  
#'    Captures average squared deviation between observed and predicted values. 
#'    RMSE penalizes larger errors more heavily. Lower values reflect better model fit.  
#'    If \code{config = TRUE}, RMSE uses the PPD; otherwise, it uses point estimates.
#'
#' 6. **Continuous Ranked Probability Score (CRPS)**  
#'    \deqn{\mathrm{CRPS}(F, y) = \int_{-\infty}^{\infty} \left[F(t) - \mathbf{1}\{y \leq t\}\right]^2 dt}  
#'    CRPS assesses how well the predictive cumulative distribution aligns with the observed outcome. 
#'    Lower scores suggest better calibrated predictive distributions. Only available when \code{config = TRUE}.
#'
#' **B) Model Comparison Metrics (relative to the first model)**
#'
#' The first model in the list is treated as the baseline for model comparisons. All other models
#' are evaluated against it using the following metrics:
#'
#' 7. **Difference in DIC and WAIC**  
#'    Stored as \code{dic_vs_first} and \code{waic_vs_first}. These represent how much higher (or lower) 
#'    each model's DIC/WAIC is compared to the first model.  
#'    Additionally, 95% credible intervals for these differences are stored as 
#'    \code{*_vs_first_lci} and \code{*_vs_first_uci}.
#'
#' 8. **Difference in MAE and RMSE**  
#'    Stored as \code{mae_vs_first} and \code{rmse_vs_first}. These reflect the absolute difference 
#'    in prediction error compared to the first model. No credible intervals are computed for these metrics.
#'
#' 9. **Continuous Ranked Probability Score Skill Score (CRPSS)**  
#'    \deqn{\mathrm{CRPSS} = 1 - \frac{\mathrm{CRPS}_{\text{model}}}{\mathrm{CRPS}_{\text{baseline}}}}  
#'    Indicates how much better the predictive distribution of the current model is 
#'    relative to the baseline model. Values closer to 1 indicate improvement; negative values 
#'    imply worse performance. Available only when \code{config = TRUE}.
#'
#' 10. **Pseudo R-squared based on deviance**  
#'     \deqn{R^2 = 1 - \exp\left( \frac{-2}{n} \left( \frac{dev_{\text{model}}}{-2} - \frac{dev_{\text{base}}}{-2} \right) \right)}  
#'     Captures relative deviance reduction compared to the baseline model. Values range from 0 
#'     (no improvement) to 1 (strong improvement).
#'
#' 11. **Random Effect Variance**  
#'     \deqn{\mathrm{Var}_{re} = \frac{1}{\mathrm{precision}}}  
#'     Quantifies residual variance due to group- or cluster-level effects. Computed only when 
#'     random effects are defined in the model formula.
#'
#' 12. **Proportional Change in Random Effect Variance**  
#'     \deqn{\frac{\mathrm{Var}_{re}}{\mathrm{Var}_{re}^{(1)}} - 1}  
#'     Represents the relative change in group-level variance compared to the baseline model. 
#'     Helps assess how much variance is explained by added covariates.
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
#' # Inspect goodness-of-fit metrics
#' results$mod_gof
#'}
#'
#' @seealso
#' \code{\link{as_GHRformulas}} converts a set of R-INLA-compatible formulas into a `GHRformulas` object.
#'
#' @export
#' @rdname fit_models
#' @name fit_models

fit_models <- function(formulas,
                       data,
                       family,
                       name,
                       offset = NULL,
                       control_compute = list(config = FALSE, vcov = FALSE),
                       nthreads = 8,
                       pb = FALSE) {
  
  # 1) Checks 
  
  # Required argument: formulas
  if (missing(formulas)) {
    stop("Argument 'formulas' is required.")
  }
  if (!inherits(formulas, "GHRformulas")) {
    stop("'formulas' must be an object of class 'GHRformulas'.")
  }
  
  # Required argument: data
  if (missing(data)) {
    stop("Argument 'data' is required.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  # Required argument: family
  if (missing(family)) {
    stop("Argument 'family' is required.")
  }
  if (!is.character(family) || length(family) != 1) {
    stop("'family' must be a single character string.")
  }
  
  # Required argument: name
  if (missing(name)) {
    stop("Argument 'name' is required.")
  }
  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a single character string.")
  }
  
  # Optional argument: offset
  if (!is.null(offset) && (!is.character(offset) || !(offset %in% names(data)))) {
    stop("If provided, 'offset' must be a character string that exists in 'data'.")
  }
  
  # control_compute must be a named list with logicals
  if (!is.list(control_compute)) {
    stop("'control_compute' must be a named list.")
  }
  if (!all(names(control_compute) %in% c("config", "vcov"))) {
    stop("'control_compute' list must only include names: 'config' and/or 'vcov'.")
  }
  if ("config" %in% names(control_compute) && !is.logical(control_compute$config)) {
    stop("'config' in 'control_compute' must be a logical value.")
  }
  if ("vcov" %in% names(control_compute) && !is.logical(control_compute$vcov)) {
    stop("'vcov' in 'control_compute' must be a logical value.")
  }
  
  # nthreads
  if (!is.numeric(nthreads) || length(nthreads) != 1 || nthreads <= 0 || nthreads != as.integer(nthreads)) {
    stop("'nthreads' must be a single positive integer.")
  }
  
  # pb
  if (!is.logical(pb) || length(pb) != 1) {
    stop("'pb' must be a single logical value (TRUE or FALSE).")
  }
  
  # Check the offset: if offset is NULL assign constant 1; if supplied but not found, stop.
  data[["offset"]] <- if (is.null(offset)) {
    1
  } else if (is.null(data[[offset]])) {
    stop("'offset' not found in the data")
  } else {
    data[[offset]]
  }
  
  # 2) Extract arguments and time 
  
  # Record start time for total runtime calculation
  total_start_time <- Sys.time()
  
  # Extract config and vcov options
  if (!("config" %in% names(control_compute))) {
    control_compute$config <- FALSE
  }
  if (!("vcov" %in% names(control_compute))) {
    control_compute$vcov <- FALSE
  }
  config   <- control_compute$config
  vcov_opt <- control_compute$vcov
  
  # Number of formulas (models) and indicator for random effects
  n <- length(formulas[["formulas"]])
  no_re <- ifelse(length(formulas$re) == 0, TRUE, FALSE)
  
  # Parameters for for 95% CI
  z.score <- 1.96        
  n_obs   <- nrow(data)
  
  # 3) Create object to store model outputs.
  mod.out <- list(
    mod_gof = data.frame(formulas[["vars"]]),
    fitted   = vector("list", length = n),
    fixed    = vector("list", length = n),
    random   = vector("list", length = n),
    vcov     = vector("list", length = n),
    formulas = rep(NA, times = n)
  )
  
  if (pb) {
    bar <- utils::txtProgressBar(min = 0, max = n, style = 3, width = n, char = "=")
  }
  init    <- numeric(n)
  finish  <- numeric(n)
  
  # We will store each model's GoF row in a list.
  gof_list <- vector("list", length = n)
  
  # 4) Model 1 (base model): Run without tryCatch
  
  init[1] <- Sys.time()
  
  # Fit the base model
  model1 <- .fit_single_model(
    formula         = formulas[["formulas"]][1],
    control_compute = list(config = config, vcov = vcov_opt),
    family          = family,
    data            = data,
    offset          = "offset",
    nthreads        = nthreads
  )
  
  gof1 <- .gof_single_model(
    fitted_model = model1,
    data         = data,
    outcome      = formulas[["outcome"]],
    config       = config,
    family       = family,
    no_re        = no_re,
    formula      = formulas
  )
  
  # Append difference columns to model 1's GoF output.
  gof_row_1 <- gof1$mod_gof |> 
    dplyr::bind_cols(
      dic_vs_first = 0,
      dic_vs_first_lci = 0, 
      dic_vs_first_uci = 0,
      waic_vs_first = 0,
      waic_vs_first_lci = 0, 
      waic_vs_first_uci = 0,
      mae_vs_first = 0,
      rmse_vs_first = 0,
      crpss = ifelse(config, 0, NA),
      r2dev = 0
    )
  
  # Capture the base model values for later comparison.
  base_values <- list(
    waic_base       = gof_row_1[["waic"]],
    dic_base        = gof_row_1[["dic"]],
    local_waic_base = model1[["waic"]][["local.waic"]],
    local_dic_base  = model1[["dic"]][["local.dic"]],
    n_local         = length(model1[["waic"]][["local.waic"]]),
    mae_base        = gof_row_1[["mae"]],
    rmse_base       = gof_row_1[["rmse"]],
    crps_base       = gof_row_1[["crps"]],
    dev_base        = model1$dic$deviance.mean
    )
  
  # Store GoF of model 1.
  gof_list[[1]] <- gof_row_1
  
  # Store Fitted from model 1
  if (config && !is.null(gof1[["ppd"]])) {
    ppd <- gof1[["ppd"]]
    mod.out[["fitted"]][[1]] <- data.frame(
      mean   = apply(ppd, 1, mean),
      median = apply(ppd, 1, stats::median),
      lci    = apply(ppd, 1, stats::quantile, probs = 0.025),
      uci    = apply(ppd, 1, stats::quantile, probs = 0.975)
    )
  } else {
    mod.out[["fitted"]][[1]] <- data.frame(
      mean   = model1[["summary.fitted.values"]][["mean"]],
      median = model1[["summary.fitted.values"]][["0.5quant"]],
      lci    = model1[["summary.fitted.values"]][["0.025quant"]],
      uci    = model1[["summary.fitted.values"]][["0.975quant"]]
    )
  }
  # Store fixed of model 1
  mod.out[["fixed"]][[1]]  <- model1[["summary.fixed"]]
  
  # Store random of model 1
  mod.out[["random"]][[1]] <- model1[["summary.random"]]
  
  # Store vcov of model 1
  if (!is.null(model1[["misc"]][["lincomb.derived.covariance.matrix"]])) {
    mod.out[["vcov"]][[1]] <- model1[["misc"]][["lincomb.derived.covariance.matrix"]]
  }
  mod.out[["formulas"]][1] <- formulas[["formulas"]][1]
  mod.out[["family"]][1]   <- family
  
  finish[1] <- Sys.time()
  if (!pb) {
    message("Model 1 completed.\n")
  } else {
    utils::setTxtProgressBar(bar, 1)
    message(sprintf("\nModel 1 of %d completed.\n", n))
  }
  
  # 5) Loop across models 2:n with tryCatch
  
  # Create a default GoF row structure based on model 1 for use in error cases.
  default_gof <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(gof_row_1)))
  colnames(default_gof) <- colnames(gof_row_1)
  
  if (n >= 2) {
    for (iteration in 2:n) {
      init[iteration] <- Sys.time()
      
      out <- tryCatch({
        model <- do.call(
          .fit_single_model,
          list(
            formula         = formulas[["formulas"]][iteration],
            control_compute = list(config = config, vcov = vcov_opt),
            family          = family,
            data            = data,
            offset          = "offset",
            nthreads        = nthreads
          )
        )
        
        gof_model <- .gof_single_model(
          fitted_model = model,
          data         = data,
          outcome      = formulas[["outcome"]],
          config       = config,
          family       = family,
          no_re        = no_re,
          formula      = formulas
        )
        
        waic_vs_first <- model[["waic"]][["waic"]] - base_values$waic_base
        dic_vs_first  <- model[["dic"]][["dic"]] - base_values$dic_base
        local_waic_model <- model[["waic"]][["local.waic"]]
        local_dic_model  <- model[["dic"]][["local.dic"]]
        dev_model <- model[["dic"]][["deviance.mean"]]
        
        waic_vs_first_se <- as.numeric(sqrt(base_values$n_local *
                                              stats::var(local_waic_model - base_values$local_waic_base, na.rm = TRUE)))
        dic_vs_first_se <- as.numeric(sqrt(base_values$n_local *
                                             stats::var(local_dic_model - base_values$local_dic_base, na.rm = TRUE)))
        
        # Gof Element
        
        gof_row <- gof_model$mod_gof |> 
          dplyr::bind_cols(
            dic_vs_first = dic_vs_first,
            dic_vs_first_lci = dic_vs_first - dic_vs_first_se * z.score,
            dic_vs_first_uci = dic_vs_first + dic_vs_first_se * z.score,
            waic_vs_first = waic_vs_first,
            waic_vs_first_lci = waic_vs_first - waic_vs_first_se * z.score,
            waic_vs_first_uci = waic_vs_first + waic_vs_first_se * z.score,
            mae_vs_first = gof_model$mod_gof$mae - base_values$mae_base,
            rmse_vs_first = gof_model$mod_gof$rmse - base_values$rmse_base,
            crpss = ifelse(config, (1 - (gof_model$mod_gof$crps / base_values$crps_base)), NA),
            r2dev = round(1 - exp((-2 / n_obs) * ((dev_model / -2) - (base_values$dev_base / -2))), 3)
          )
        
        # Fitted values 
        if (config && !is.null(gof_model[["ppd"]])) {
          ppd <- gof_model[["ppd"]]
          mod.out[["fitted"]][[iteration]] <- data.frame(
            mean   = apply(ppd, 1, mean),
            median = apply(ppd, 1, stats::median),
            lci    = apply(ppd, 1, stats::quantile, probs = 0.025),
            uci    = apply(ppd, 1, stats::quantile, probs = 0.975)
          )
        } else {
          mod.out[["fitted"]][[iteration]] <- data.frame(
            mean   = model[["summary.fitted.values"]][["mean"]],
            median = model[["summary.fitted.values"]][["0.5quant"]],
            lci    = model[["summary.fitted.values"]][["0.025quant"]],
            uci    = model[["summary.fitted.values"]][["0.975quant"]]
          )
        }
        
        # Fixed effects
        mod.out[["fixed"]][[iteration]]  <- model[["summary.fixed"]]
        
        #Random effects
        mod.out[["random"]][[iteration]] <- model[["summary.random"]]
        if (!is.null(model[["misc"]][["lincomb.derived.covariance.matrix"]])) {
          mod.out[["vcov"]][[iteration]] <- model[["misc"]][["lincomb.derived.covariance.matrix"]]
        }
        gof_row
      }, error = function(e) {
        message(sprintf("INLA returned an error, model failed to run. Skipping Model %d.", iteration))       
        mod.out[["fitted"]][[iteration]] <- NULL
        mod.out[["fixed"]][[iteration]] <- NULL
        mod.out[["random"]][[iteration]] <- NULL
        mod.out[["vcov"]][[iteration]] <- NULL
        default_gof
      })
      
      # tryCatch finish here , now storing the output 
      
      gof_list[[iteration]] <- out
      mod.out[["formulas"]][iteration] <- formulas[["formulas"]][iteration]
      mod.out[["family"]][iteration] <- family
      finish[iteration] <- Sys.time()
      
      if (pb) {
        utils::setTxtProgressBar(bar, iteration)
        time <- round((sum(finish - init) / 60), 2)
        message(sprintf("\nModel %d of %d completed. Total run time: %s minutes.\n", 
                    iteration, n, time))
      } else {
        message(sprintf("\nModel %d of %d completed.\n", iteration, n))
      }
    }
  }
  
  if (pb) { close(bar) }
  
  # 6) Postprocess and combine model output 
  
  # Combine the individual GoF rows into one data frame.
  mod_gof_all <- dplyr::bind_rows(gof_list)
  
  # Optionally compute changes across random effects.
  mod_gof_all <- mod_gof_all |>
    dplyr::mutate(base_row = (dplyr::row_number() == 1)) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("_var"),
        function(x) (x - dplyr::first(x)) / dplyr::first(x),
        .names = "{.col}_change"
      )
    ) |>
    dplyr::select(-all_of("base_row"))
  
  # Attach a model ID to each model.
  mod_gof_all$model_id <- paste0(name, seq_len(nrow(mod_gof_all)))
  
  # Store the final combined GoF output directly.
  mod.out$mod_gof <- cbind(mod.out$mod_gof, mod_gof_all)
  
  # Relocate model_id and any "_var" columns.
  mod.out[["mod_gof"]] <- mod.out[["mod_gof"]] |>
    dplyr::relocate(all_of("model_id"), .before = 1) |>
    dplyr::relocate(tidyselect::contains("_var"), .after = dplyr::last_col())

  # Name the lists according to the model IDs.
  names(mod.out[["formulas"]]) <- mod_gof_all$model_id
  names(mod.out[["fitted"]])   <- mod_gof_all$model_id
  names(mod.out[["fixed"]])    <- mod_gof_all$model_id
  names(mod.out[["random"]])   <- mod_gof_all$model_id
  names(mod.out[["vcov"]])     <- mod_gof_all$model_id
  
  mod.out[["data"]]    <- data
  mod.out[["re"]]      <- formulas[["re"]]
  mod.out[["outcome"]] <- formulas[["outcome"]]
  
  class(mod.out) <- append("GHRmodels", class(mod.out))
  
  # 7) Return output 
  return(mod.out)
}
