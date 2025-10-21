#_______________________________________________________________________________
# Helper Functions for Internal Checks ----------------------------------------
#_______________________________________________________________________________
#' Fit a Single INLA Model
#'
#' This function fits a single Bayesian model using the \code{R-INLA} package. It is  
#' used internally by \code{\link{fit_models}} but can be called directly to fit and 
#' inspect a single model at a time.
#'
#' @param formula A \strong{formula} or character string specifying the model to be fitted. 
#'   If a character string is provided, it will be converted to a formula via \code{stats::as.formula()}.
#' @param data A \strong{data.frame} containing all variables referenced in \code{formula} and \code{offset}.
#' @param family A character string specifying the likelihood family for the INLA model 
#'   (e.g., \code{"nbinomial"}, \code{"poisson"}). Defaults to \code{"nbinomial"}.
#' @param offset Either:
#'   \describe{
#'     \item{Character string}{In \code{data} there must be a variable matching the string 
#'       provided. For example, \code{"offset"} references a variable \code{data$offset}.}
#'     \item{Numeric vector}{A numeric vector of offset values (same length as \code{nrow(data)}). 
#'       Internally, \code{log(offset\_values)} is applied.}
#'   }
#'   If \code{NULL}, no offset is applied. Defaults to \code{"offset"}. 
#' @param control_compute A named list controlling additional computation options:
#'   \describe{
#'     \item{\code{config}}{Logical indicating whether to store extra internal configuration 
#'       (e.g., enabling posterior predictive checks like CRPS). Defaults to \code{FALSE}.}
#'     \item{\code{vcov}}{Logical indicating whether to return the variance-covariance 
#'       (correlation) matrix of fixed effects. Defaults to \code{FALSE}.}
#'   }
#' @param nthreads Integer specifying the number of threads for parallel computation. 
#'   Must be a positive integer. Defaults to \code{8}.
#'
#' @return A fitted \code{inla} model object (an S3 object of class \code{inla}).
#'
#' @examples
#' \donttest{
#' # Create a simple dataset
#' my_data <- data.frame(
#'   y = rpois(100, lambda = 10),
#'   x = rnorm(100),
#'   offset_var = rep(1, 100)
#' )
#'
#' # Example formula
#' my_formula <- y ~ x
#'
#' # Fit a single Poisson model with offset
#' model_result <- fit_single_model(
#'   formula = my_formula,
#'   data = my_data,
#'   family = "poisson",
#'   offset = "offset_var",
#'   control_compute = list(config = TRUE, vcov = TRUE),
#'   nthreads = 2
#' )
#' }
#'
#' @seealso
#' \code{\link{fit_models}} for fitting multiple INLA models.
#' \code{\link{write_inla_formulas}} for writing INLA formulas.
#' \href{https://www.r-inla.org/}{R-INLA documentation}.
#'
#' @noRd

.fit_single_model <- function(formula,
                              data,
                              family = "nbinomial",
                              offset = "offset",
                              control_compute = list(config = FALSE,
                                                     vcov = FALSE),
                              nthreads = 8) {
  
  
  # INLA check
  if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    stop("Package 'INLA' is required. Please install it from https://inla.r-inla-download.org")
  }
  
  # Arguments check
  if (missing(formula) || missing(data)) {
    stop("'formula' and 'data' must be provided")
  }
  
  # Extract config and vcov
  check_control_compute <- names(control_compute)
  
  if (!("config" %in% check_control_compute)) {
    control_compute$config <- FALSE
  }
  
  if (!("vcov" %in% check_control_compute)) {
    control_compute$vcov <- FALSE
  }
  
  config<- control_compute$config
  vcov<- control_compute$vcov
  
  
  # Convert 'forms' to formula if it's a character string
  if (is.character(formula)) {
    formula <- tryCatch(
      stats::as.formula(formula),
      error = function(e) {
        stop("'form' could not be converted to a valid formula. Please check the syntax.")
      }
    )
  }
  
  # Extract offset values
  offset_vals <- log(data[[offset]])
  
  #Fit model
  INLA::inla(
    formula = formula,
    family = family,
    offset = offset_vals,
    control.inla = list(strategy = "adaptive"),
    control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = config),
    control.predictor = list(link = 1, compute = TRUE),
    verbose = FALSE,
    INLA::inla.setOption(num.threads = nthreads),
    control.fixed = list(
      # mean.intercept=0, # Intercept mean
      # prec.intercept=1, # Intercep precision 
      # mean=0,  # Coefficient mean
      # prec=1,  # Coefficient precision
      correlation.matrix = vcov),
    data = data
  )
}

#' Extract Goodness-of-Fit Metrics from a Fitted INLA Model
#'
#' This function extracts various goodness-of-fit (GoF) metrics from a fitted \code{INLA} model object. It is primarily 
#' intended for internal use by \code{\link{fit_models}}, but can also be called independently if desired.
#'
#' @param fitted_model An \code{inla} model object representing the fitted model.
#' @param data A \code{data.frame} containing the dataset used to fit the model.
#' @param outcome A character string specifying the name of the outcome variable in \code{data}.
#' @param config Logical; \code{config = TRUE} enables sampling from the posterior 
#' predictive distribution (needed for posterior predictive checks like the **CRPS** metric). 
#' Defaults to \code{FALSE}.
#' @param ci Logical; if \code{TRUE}, confidence intervals for DIC and WAIC are computed. Defaults to \code{TRUE}.
#' @param family A character string specifying the likelihood family (e.g., \code{"nbinomial"}, \code{"poisson"}). 
#'   Defaults to \code{"nbinomial"}.
#' @param no_re Logical; if \code{TRUE}, random effect variances are not extracted. Defaults to \code{TRUE}.
#' @param formula (Optional) A \code{GHRformulas} or similar object containing model formulas. 
#'   Required if \code{no_re = FALSE} to identify random effects for variance extraction.
#'
#' @details
#' This function is primarily used internally by \code{\link{fit_models}} to gather GoF metrics 
#' across multiple fitted INLA models. However, it can also be used directly if you have a single 
#' fitted model and want a structured summary of key GoF metrics.  
#'
#' For details about the GoF metrics extracted by this function see \code{\link{fit_models}} documentation.
#'  
#' *Note*: If \code{no_re = FALSE}, random effect variances are extracted from \code{fitted_model} 
#' using the model structure defined in a given \code{GHRformula}.
#'
#' @return A list with one component, \code{mod_gof}, containing all extracted goodness-of-fit metrics. 
#'
#' @examples
#' \donttest{
#' # Create a simple dataset
#' data <- data.frame(
#'   y = rpois(100, lambda = 10),
#'   x = rnorm(100),
#'   time_id = seq(1, 100, by = 1)
#'   offset_var = rep(1, 100))
#' )
#'
#' # Example formula
#' formula <- y ~ x + f(time_id, model = 'rw1')
#'
#' # Fit a single Poisson model with offset
#' model_inla <- fit_single_model(
#'   formula = formula,
#'   data = data,
#'   family = "poisson",
#'   offset = "offset_var",
#'   config = TRUE,
#'   nthreads = 2
#' )
#' 
#' results <- gof_single_model(
#'   fitted_model = model_inla,
#'   data = data,
#'   outcome = 'y',
#'   config = TRUE,
#'   family = 'poisson',
#'   no_re = TRUE
#' )
#'
#' }
#'
#' @seealso
#' \code{\link{fit_models}} for fitting multiple INLA models 
#'
#' @noRd


.gof_single_model <- function(fitted_model,
                              data,
                              outcome, 
                              config = FALSE,
                              ci = TRUE,
                              family = "nbinomial",
                              formula = NULL,
                              no_re = TRUE) {
  
  # Check required arguments
  if (missing(fitted_model) || missing(data) || missing(outcome)){
    stop("Error: one between 'fitted_models', 'data', 'otucome' arguments not provided")
  }
  
  # Check if no_re == FALSE GHRformulas are required 
  if (no_re == FALSE && is.null(formula)) {
    stop("'no_re' can be TRUE only if the corresponding GHRformulas object is provided")
  }
  
  # Extract information form the models depending if random effects exists
  if (no_re == TRUE) {
    mod.out <- list(
      mod_gof = dplyr::bind_cols(
        .extract_DIC_inla(model = fitted_model),
        .extract_WAIC_inla(model = fitted_model),
        .extract_LMS_inla(model = fitted_model))
    )
  } else {
    mod.out <- list(
      mod_gof = dplyr::bind_cols(
        .extract_re_var_inla(model = fitted_model, formulas = formula),
        .extract_DIC_inla(model = fitted_model),
        .extract_WAIC_inla(model = fitted_model),
        .extract_LMS_inla(model = fitted_model))
    )
  }
  
  # Sample from posterior predictive distribution and extract predictive metrics
  if (config == TRUE) {
    
    # Draw samples from posterior predictive distribution
    if (family == "nbinomial") {
      ppd <- .ppd_nb(fitted_model, s = 1000)
    } else if (family == "poisson") {
      ppd <- .ppd_pois(fitted_model, s = 1000)
    } else {
      stop("Error: likelihood family not supported for posterior sampling")
    }
    
    mod.out[["ppd"]] <- ppd
    
    mae_df  <- .extract_MAE_ppd(ppd = ppd, data = data, cases = outcome)
    rmse_df <- .extract_RMSE_ppd(ppd = ppd, data = data, cases = outcome)
    crps    <- .extract_CRPS_ppd(ppd = ppd, data = data, cases = outcome)
    #r2var   <- .extract_R2var_ppd(ppd = ppd, data = data, cases = outcome)
    
    mod.out[["mod_gof"]] <- mod.out[["mod_gof"]] |> dplyr::bind_cols(
      mae  = stats::median(mae_df$mae),
      rmse = stats::median(rmse_df$rmse),
      crps = crps
      #,r2var = r2var
    )
    
  } else {
    
    # Use summary.fitted.values when config = FALSE
    mae_df  <- .extract_MAE_inla(model = fitted_model, data = data, cases = outcome)
    rmse_df <- .extract_RMSE_inla(model = fitted_model, data = data, cases = outcome)
    
    mod.out[["mod_gof"]] <- mod.out[["mod_gof"]] |> dplyr::bind_cols(
      mae  = stats::median(mae_df$mae),
      rmse = stats::median(rmse_df$rmse),
      crps = NA
      #,r2var = NA
    )
  }
  
  # Return 
  return(mod.out) 
}

