#_______________________________________________________________________________
# Helper Functions for Extracting information from fitted R-INLA models  -------
#_______________________________________________________________________________

# extract_DIC_inla -------------------------------------------------------------
#' Extract Deviance Information Criterium (DIC) from \code{inla} Model
#'
#' This internal function retrieves the Deviance Information Criterion (DIC) from a fitted INLA model.
#' Optionally, it can also compute confidence intervals for the DIC based on the variability
#' of the point-wise DIC values.
#'
#' @param model An object of class \code{inla} representing the fitted \code{inla} model.
#' @param ci Logical. If \code{TRUE}, computes confidence intervals for the DIC. Default is \code{TRUE}.
#'
#' @return A \code{data.frame} containing the DIC value. If \code{ci = TRUE}, it also includes
#'         the lower and upper confidence intervals (\code{dic_lci} and \code{dic_uci}).
#'
#' @noRd

.extract_DIC_inla <- function(model,
                              ci = FALSE) {
  
  # Set z-score (for now for CIs of 95%)
  z.score <- 1.96
  
  # Extract DIC value
  dic <- model[["dic"]][["dic"]]
  
  # Extract local DIC values
  local_dic <- model[["dic"]][["local.dic"]]
  
  # Calculate number of observations
  n <- length(local_dic)
  
  # Calculate standard error of DIC
  s.e <- sqrt(n * stats::var(local_dic, na.rm = TRUE))
  
  # Prepare output based on 'ci' parameter
  if(ci){
    out <- data.frame(
      dic = dic,
      dic_lci = dic - s.e * z.score, # Lower CI
      dic_uci = dic + s.e * z.score  # Upper CI
    )
  } else{
    out <- data.frame(
      dic = dic
    )
  }
  
  return(out)
}

# extract_WAIC_inla -------------------------------------------------------------

#' Extract the Watanabe–Akaike Information Criterion (WAIC) from an \code{inla} Model
#'
#' This internal function retrieves the Watanabe–Akaike Information Criterion (WAIC) from a fitted \code{inla} model.
#' Optionally, it can also compute confidence intervals for the WAIC based on the variability
#' of the point-wise WAIC values.
#'
#' @param model An object of class \code{inla} representing the fitted \code{inla} model.
#' @param ci Logical. If \code{TRUE}, computes confidence intervals for the WAIC. Default is \code{TRUE}.
#'
#' @return A \code{data.frame} containing the WAIC value. If \code{ci = TRUE}, it also includes
#'         the lower and upper confidence intervals (\code{waic_lci} and \code{waic_uci}).
#'
#' @noRd

.extract_WAIC_inla <- function(model,
                               ci = FALSE) {
  
  # Set z-score for 95% confidence interval
  z.score <- 1.96
  
  # Extract WAIC value
  waic <- model[["waic"]][["waic"]]
  
  # Extract local WAIC values
  local_waic <- model[["waic"]][["local.waic"]]
  
  # Calculate number of observations
  n <- length(local_waic)
  
  # Calculate standard error of WAIC
  s.e <- sqrt(n * stats::var(local_waic, na.rm = TRUE))
  
  # Prepare output based on 'ci' parameter
  if(ci){
    out <- data.frame(
      waic = waic,
      waic_lci = waic - s.e * z.score, # Lower CI
      waic_uci = waic + s.e * z.score  # Upper CI
    )
  } else{
    out <- data.frame(
      waic = waic
    )
  }
  
  return(out)
}

# extract_LMS_inla -------------------------------------------------------------
#' Extract Log Mean Score (LMS) from an \code{inla} Model
#'
#' This internal function computes the Log Mean Score (LMS) from a fitted INLA model.
#' LMS is calculated as the mean of the negative logarithm of the Conditional Predictive Ordinate (CPO) values.
#'
#' @param model An object of class \code{inla} representing the fitted INLA model.
#'
#' @return A \code{data.frame} containing the Log Mean Score (\code{lms}).
#'
#' @noRd

.extract_LMS_inla <- function(model) {
  
  # Extract CPO values
  cpo_values <- model[["cpo"]][["cpo"]]
  
  # Compute Log Mean Score
  lms <- mean(-log(cpo_values), na.rm = TRUE)
  
  # Return as data.frame
  out <- data.frame(
    lms = lms
  )
  
  return(out)
}

# extract_MAE_inla -------------------------------------------------------------
#' Extract Mean Absolute Error (MAE) from an \code{inla} Model
#'
#' This internal function calculates the Mean Absolute Error (MAE) between the observed values and the median
#' of the fitted values from a fitted INLA model.
#'
#' @param model An object of class \code{inla} representing the fitted \code{inla} model.
#' @param data A \code{data.frame} containing the dataset used to fit the model. Must include the
#'             observed outcome variable specified in \code{cases}.
#' @param cases Character string. The name of the variable in \code{data} representing the observed outcomes.
#'
#' @return A \code{data.frame} containing the Mean Absolute Error (\code{mae}).
#'
#' @noRd


.extract_MAE_inla <- function(model,
                              data,
                              cases) {
  
  # Extract observed and predicted values
  observed <- data[[cases]]
  predicted <- model[["summary.fitted.values"]][["0.5quant"]]
  
  # Calculate absolute errors
  abs_error <- abs(predicted - observed)
  
  # Calculate MAE
  mae <- mean(abs_error, na.rm = TRUE)
  
  # Return as data.frame
  out <- data.frame(
    mae = mae
  )
  
  return(out)
}

# extract_RMSE_inla -------------------------------------------------------------
#' Extract Root Mean Squared Error (RMSE) from an \code{inla} Model
#'
#' This internal function calculates the Root Mean Squared Error (RMSE) between the observed values and the median
#' of the fitted values from a fitted \code{inla} model.
#'
#' @param model An object of class \code{inla} representing the fitted \code{inla} model.
#' @param data A \code{data.frame} containing the dataset used to fit the model. Must include the
#'             observed outcome variable specified in \code{cases}.
#' @param cases Character string. The name of the variable in \code{data} representing the observed outcomes.
#'
#' @return A \code{data.frame} containing the Root Mean Squared Error (\code{rmse}).
#'
#' @noRd


.extract_RMSE_inla <- function(model,
                               data,
                               cases) {
  
  # Extract observed and predicted values
  observed <- data[[cases]]
  predicted <- model[["summary.fitted.values"]][["0.5quant"]]
  
  # Calculate squared errors
  sq_errors <- (observed - predicted)^2
  
  # Calculate RMSE
  rmse <- sqrt(mean(sq_errors, na.rm = TRUE))
  
  # Return as data.frame
  out <- data.frame(
    rmse = rmse
  )
  
  return(out)
}


# post_pred_nb ----------------------------------------------------------------

#' Sampling from a Posterior Predictive Distribution of an INLA model with
#' a Negative Binomial Likelihood
#' @param model an INLA model
#' @param s number of samples
#' @return Posterior Predictive Distribution
#'
#' @noRd

.ppd_nb <- function(model = NULL, s = 1000) {
  
  # INLA check
  if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    stop("Package 'INLA' is required. Please install it from https://inla.r-inla-download.org")
  }
  
  xx <- INLA::inla.posterior.sample(s, model)
  
  # Extract the overdispersion parameter 
  theta <- unlist(
    lapply(
      xx,
      function(x) as.numeric(x[["hyperpar"]][
        grepl("overdispersion", names(x[["hyperpar"]]))
      ])
    )
  )
  
  # Obtain the expectation of counts per row
  latent <- lapply(
    xx,
    function(x) as.numeric(x[["latent"]][
      grepl("Predictor", rownames(x[["latent"]]))
    ])
  )
  
  n_obs <- length(latent[[1]])
  # Create posterior predictive sample from a negative binomial distribution
  y_pred <- matrix(NA, n_obs, s)
  for(s.idx in 1:s) {
    y_pred[, s.idx] <- stats::rnbinom(n_obs,
                                      mu = exp(latent[[s.idx]]), # Predicted means
                                      size = theta[s.idx]) # Overdispersion parameter
  }
  
  return(y_pred)
}

# post_pred_pois ----------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model 
#' with Poisson Likelihood
#' @param model an INLA model
#' @param s number of samples
#' @return Posterior Predictive Distribution
#' 
#' @noRd

.ppd_pois <- function(model = NULL, s = 1000){
  
  # Extract the linear predictor (eta) for all observations
  xx <- INLA::inla.posterior.sample(s, model)
  
  
  latent <- lapply(
    xx,
    function(x) as.numeric(x[["latent"]][
      grepl("Predictor", rownames(x[["latent"]]))
    ])
  )
  
  n_obs <- length(latent[[1]])
  
  # Create posterior predictive sample from a poisson distribution
  y_pred <- matrix(NA, n_obs, s)
  for(s.idx in 1:s) {
    y_pred[, s.idx] <- stats::rpois(n_obs,
                                    lambda = exp(latent[[s.idx]])) # Predicted means
  }
  return(y_pred)
}

# crps_edf --------------------------------------------------------------------
#' Compute Continuous Ranked Probability Score (CRPS) from Empirical 
#' Distribution Function (EDF). It evaluates the accuracy of a predictive
#' distribution against observed values.
#'
#' @param obs A numeric vector of observed values.
#' @param pred A matrix where each row contains probabilistic predictions for 
#' the corresponding observed value in `obs`. 
#' @return A numeric vector containing the CRPS for each observed value.
#' 
#' @noRd


.crps_edf <- function(obs, pred) {
  # Check that the number of rows in `pred` matches the length of `obs`
  if (nrow(pred) != length(obs)) {
    stop("In CRPS evaluation the number of rows in `pred` must match the length of `obs`.")
  }
  
  # CRPS computation for each observation
  sapply(seq_along(obs), function(i) {
    
    # Sort predictions to compute the EDF for the current observation
    sorted_pred <- sort(pred[i, ])
    
    # Normalization constant for the EDF
    n <- length(sorted_pred)
    c_1n <- 1 / n  
    #Compute the EDF probabilities 
    edf_probs <- seq(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = n)  
    
    # Compute CRPS for the current observation
    sum(2 * c_1n * ((obs[i] < sorted_pred) - edf_probs) * (sorted_pred - obs[i]))
  })
}

# extract_re_var_inla ----------------------------------------------------------
#' This internal function extracts the variances of the random effects from a fitted INLA model.
#' It processes the hyperparameters summary to retrieve precision parameters and computes
#' their inverses to obtain variances.
#'
#' @param model An object of class \code{inla} representing the fitted INLA model.
#' @param formulas A list containing model formulas, specifically with a component \code{re}
#'                 that is a character vector of random effect names as specified in the model.
#'
#' @return A \code{data.frame} where each column corresponds to the variance of a random effect.
#'         Column names are derived from the random effect names appended with "_var".
#'
#' @noRd

.extract_re_var_inla <- function(model = NULL,
                                 formulas = NULL) {
  #Extract random effects 
  re <- sub("^f\\(([^,)]+).*", "\\1", formulas$re)
  
  #Extract summary of hyperparameters 
  hyperpar <- model[["summary.hyperpar"]]
  
  # Add row names as a column for filtering
  hyperpar$Parameter <- rownames(hyperpar)
  
  precision_index <- grep("Precision",hyperpar$Parameter)
  precision_re_index <- which(sapply(hyperpar$Parameter[precision_index], function(line) {
    any(sapply(re, function(id) grepl(id, line)))}))
  precision_median <- hyperpar[precision_index[precision_re_index],c("0.5quant")]
  variance_re <-  1/precision_median
  
  col_names<-paste0(names(formulas$re),"_var")
  n_re <- length(formulas$re)
  
  
  out <- as.data.frame(matrix(ncol = n_re, nrow = 1)) 
  names(out) <- col_names
  out[1, ] <- variance_re
  
  return(out)
}


#' Compute MAE from a Posterior Predictive Distribution
#'
#' @param ppd A numeric matrix where each column is a sample of the ppd.
#' @param data A data frame containing the observed values.
#' @param cases A character string indicating the column in \code{data} that holds observed values.
#'
#' @return A \code{data.frame} containing the Mean Absolute Error (\code{mae}) for each column of \code{ppd}.
#'
#' @noRd 

.extract_MAE_ppd <- function(ppd, data, cases) {
  
  # Extract observed values
  observed <- data[[cases]]
  
  # Check dimensions
  if (nrow(ppd) != length(observed)) {
    stop("Number of rows in 'ppd' must match length of observed 'cases'.")
  }
  
  # For each column in ppd, compute MAE = mean of absolute differences
  mae_samples <- apply(ppd, 2, function(x) {
    mean(abs(x - observed), na.rm = TRUE)
  })
  
  # Return as data.frame
  out <- data.frame(
    mae = mae_samples
  )
  return(out)
}


#' Compute RMSE from a Posterior Predictive Distribution
#'
#' @param ppd A numeric matrix where each column is a sample of the ppd.
#' @param data A data frame containing the observed values.
#' @param cases   A numeric vector of length equal to nrow(obs_mat),
#'                representing the observed values.
#'
#' A \code{data.frame} containing the Root Mean Squared Error (\code{rmse}).
#' 
#' @noRd

.extract_RMSE_ppd <- function(ppd, data, cases) {
  
  # Extract observed and predicted values
  observed <- data[[cases]]
  
  # Check dimensions
  if (nrow(ppd) != length(observed)) {
    stop("Number of rows in 'ppd' must match length of 'cases'.")
  }
  
  # Apply over columns (MARGIN = 2)
  # For each column x, compute sqrt(mean((x - cases)^2 ))
  
  rmse_samples <- apply(ppd, 2, function(x) {
    sqrt(mean((x - observed)^2, na.rm = TRUE))
  })
  
  # Return as data.frame
  out <- data.frame(
    rmse = rmse_samples
  )
  return(out)
}

# extract_CRPS_inla ------------------------------------------------------------
#' Calculate CRPS
#' @param data the dataframe used to fit the model.
#' @param cases character, specifying the name of variable for cases
#' @param ppd matrix, the posterior predictive distribution
#' @return model crps
#'
#' @noRd

.extract_CRPS_ppd <- function(ppd, data, cases) {
  
  # retrieve observed 
  observed <- data[[cases]]
  
  # Compute the overall CRPS 
  crps <- stats::median(.crps_edf(obs = observed, pred =  ppd), na.rm = TRUE)
  
  out <- data.frame(
    crps = crps
  )
  
  return(crps)
}


#' Compute Bayesian R² distribution across posterior draws (Gelman et al. 2019)
#'
#' @param ppd A matrix of posterior predictive draws: rows = observations, columns = draws
#' @param data A data frame with the observed outcome
#' @param cases A string, name of the observed outcome variable in \code{data}
#'
#' @return A data frame with posterior summary: median and 95% CI of R²
#'
#' @noRd
.extract_R2var_ppd <- function(ppd, data, cases) {
  
  # Observed outcomes
  y_obs <- data[[cases]]
  
  # Number of posterior draws
  S <- ncol(ppd)
  
  # Compute R² for each posterior draw
  r2_vec <- sapply(1:S, function(s) {
    y_pred_s <- ppd[, s]
    res_s    <- y_obs - y_pred_s
    
    var_fit  <- stats::var(y_pred_s, na.rm = TRUE)
    var_res  <- stats::var(res_s, na.rm = TRUE)
    
    r2_s <- var_fit / (var_fit + var_res)
    return(r2_s)
  })
  
  # Summarize R² posterior
  out <- data.frame(
    r2var = stats::median(r2_vec, na.rm = TRUE)
  )
  
  return(out)
}
