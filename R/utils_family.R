# .sample_ppd ----------------------------------------------------------------
#' Sample from a Posterior Predictive Distribution of an INLA model. Wrapper
#' for the different families available in GHRmodel.
#'
#' @param inla_object an INLA model
#' @param family number of samples
#' @param ppd_samples number of samples for the ppd
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.sample_ppd <- function(
  inla_object,
  family,
  ppd_samples,
  selection = list(),
  nthreads = 1
) {
  if (family == "nbinomial") {
    preds <- .ppd_nb(
      inla_object,
      nsamples = ppd_samples,
      selection = selection,
      nthreads = nthreads
    )
  } else if (family == "poisson") {
    preds <- .ppd_pois(
      inla_object,
      nsamples = ppd_samples,
      selection = selection,
      nthreads = nthreads
    )
  } else if (family == "gaussian") {
    preds <- .ppd_gaussian(
      inla_object,
      nsamples = ppd_samples,
      selection = selection,
      nthreads = nthreads
    )
  } else if (family == "zeroinflatedpoisson0") {
    preds <- .ppd_zipois(
      inla_object,
      nsamples = ppd_samples,
      selection = selection,
      nthreads = nthreads
    )
  } else if (family == "zeroinflatednbinomial0") {
    preds <- .ppd_zinb(
      inla_object,
      nsamples = ppd_samples,
      selection = selection,
      nthreads = nthreads
    )
  } else {
    stop(
      "Error: likelihood family not supported for ppd 'pred_type'"
    )
  }
  return(preds)
}

# .ppd_gaussian ----------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model with
#' a Gaussian Likelihood
#' @param inla_object an INLA model
#' @param nsamples number of samples
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.ppd_gaussian <- function(
  inla_object,
  nsamples = 1000,
  selection = list(),
  nthreads = 1
) {
  # Extract the linear predictor for all observations
  post_samples <- INLA::inla.posterior.sample(
    nsamples,
    inla_object,
    selection = selection,
    num.threads = nthreads
  )

  # Extract the precision parameter
  precision <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["hyperpar"]][
        grepl("Precision", names(x[["hyperpar"]]))
      ])
    }
  )

  # Convert to standard deviation
  sigma <- 1 / sqrt(unlist(precision))

  # Obtain the expectation of counts per row
  latent <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    }
  )

  # Create posterior predictive sample from a negative binomial distribution
  n_obs <- length(latent[[1]])
  y_pred <- matrix(NA, n_obs, nsamples)
  for (s.idx in 1:nsamples) {
    y_pred[, s.idx] <- stats::rnorm(
      n_obs,
      mean = latent[[s.idx]], # Predicted means
      sd = sigma[s.idx] # variance
    )
  }

  return(y_pred)
}

# .ppd_nb ----------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model with
#' a Negative Binomial Likelihood
#' @param inla_object an INLA model
#' @param nsamples number of samples
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.ppd_nb <- function(
  inla_object,
  nsamples = 1000,
  selection = list(),
  nthreads = 1
) {
  # Extract the linear predictor for all observations
  post_samples <- INLA::inla.posterior.sample(
    nsamples,
    inla_object,
    selection = selection,
    num.threads = nthreads
  )

  # Extract the overdispersion parameter
  theta <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["hyperpar"]][
        grepl("size for the nbinomial", names(x[["hyperpar"]]))
      ])
    }
  )

  # Obtain the expectation of counts per row
  latent <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    }
  )

  # Create posterior predictive sample from a negative binomial distribution
  n_obs <- length(latent[[1]])
  y_pred <- matrix(NA, n_obs, nsamples)
  for (s.idx in 1:nsamples) {
    y_pred[, s.idx] <- stats::rnbinom(
      n_obs,
      mu = exp(latent[[s.idx]]), # Predicted means
      size = theta[[s.idx]] # Overdispersion parameter
    )
  }

  return(y_pred)
}

# .ppd_zinb ----------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model with
#' a zero-inflated Negative Binomial Likelihood
#' @param inla_object an INLA model
#' @param nsamples number of samples
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.ppd_zinb <- function(
  inla_object,
  nsamples = 1000,
  selection = list(),
  nthreads = 1
) {
  # Extract the linear predictor for all observations
  post_samples <- INLA::inla.posterior.sample(
    nsamples,
    inla_object,
    selection = selection,
    num.threads = nthreads
  )

  # Extract the ZIP parameter
  p <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["hyperpar"]][
        grepl("zero-probability", names(x[["hyperpar"]]))
      ])
    }
  )

  # Extract the overdispersion parameter
  theta <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["hyperpar"]][
        grepl("size for nbinomial_0", names(x[["hyperpar"]]))
      ])
    }
  )

  # Obtain the expectation of counts per row
  latent <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    }
  )

  # Create posterior predictive sample from a negative binomial distribution
  n_obs <- length(latent[[1]])
  y_pred <- matrix(NA, n_obs, nsamples)
  for (s.idx in 1:nsamples) {
    y_pred[, s.idx] <- VGAM::rzinegbin(
      n_obs,
      mu = exp(latent[[s.idx]]), # Predicted means
      size = theta[[s.idx]], # Overdispersion parameter
      pstr0 = p[[s.idx]] # ZI parameter
    )
  }

  return(y_pred)
}


# .ppd_pois ---------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model
#' with Poisson Likelihood
#' @param inla_object an INLA model
#' @param nsamples number of samples
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.ppd_pois <- function(
  inla_object,
  nsamples = 1000,
  selection = list(),
  nthreads = 1
) {
  # Extract the linear predictor for all observations
  post_samples <- INLA::inla.posterior.sample(
    nsamples,
    inla_object,
    selection = selection,
    num.threads = nthreads
  )

  # Obtain the expectation of counts per row
  latent <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    }
  )

  # Create posterior predictive sample from a poisson distribution
  n_obs <- length(latent[[1]])
  y_pred <- matrix(NA, n_obs, nsamples)
  for (s.idx in 1:nsamples) {
    y_pred[, s.idx] <- stats::rpois(n_obs, lambda = exp(latent[[s.idx]]))
  }
  return(y_pred)
}

# .ppd_zipois ---------------------------------------------------------------
#' Sampling from a Posterior Predictive Distribution of an INLA model
#' with Zero-inflated Poisson Likelihood
#' @param inla_object an INLA model
#' @param nsamples number of samples
#' @param selection list used to sample from specific rows
#' @param nthreads maximum number of threads used for sampling
#' @return Posterior Predictive Distribution
#'
#' @noRd
.ppd_zipois <- function(
  inla_object,
  nsamples = 1000,
  selection = list(),
  nthreads = 1
) {
  # Extract the linear predictor for all observations
  post_samples <- INLA::inla.posterior.sample(
    nsamples,
    inla_object,
    selection = selection,
    num.threads = nthreads
  )

  # Extract the ZIP parameter
  p <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["hyperpar"]][
        grepl("zero-probability", names(x[["hyperpar"]]))
      ])
    }
  )

  # Obtain the expectation of counts per row
  latent <- lapply(
    post_samples,
    function(x) {
      as.numeric(x[["latent"]][
        grepl("Predictor", rownames(x[["latent"]]))
      ])
    }
  )

  # Create posterior predictive sample from a poisson distribution
  n_obs <- length(latent[[1]])
  y_pred <- matrix(NA, n_obs, nsamples)
  for (s.idx in 1:nsamples) {
    y_pred[, s.idx] <- VGAM::rzipois(
      n_obs,
      lambda = exp(latent[[s.idx]]), # Predicted means
      pstr0 = p[[s.idx]] # ZI parameter
    )
  }
  return(y_pred)
}
