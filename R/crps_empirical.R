# crps_empirical ----------------------------------------------------------
#' Compute the empirical Continuous Ranked Probability Score (CRPS)
#'
#' Computes the CRPS for a single observation given a set of posterior
#' predictive draws, using the empirical (energy form) estimator
#' \deqn{\mathrm{CRPS} = \frac{1}{n} \sum_{i=1}^n |x_i - y| -
#' \frac{1}{2n^2} \sum_{i=1}^n \sum_{j=1}^n |x_i - x_j|}
#' where \eqn{y} is the observed value and \eqn{x_1, \dots, x_n} are the
#' posterior predictive draws. Missing draws in \code{pred_pdd} are dropped
#' before computation.
#'
#' @param y A single numeric observed value. Returns \code{NA_real_} if
#' missing.
#' @param pred_pdd A numeric vector of posterior predictive draws for the
#' observation. Returns \code{NA_real_} if empty or all missing.
#'
#' @return A single numeric CRPS value (or \code{NA_real_}, see above).
#'
#' @references
#' Gneiting, T. and Raftery, A. E. (2007). Strictly Proper Scoring Rules,
#' Prediction, and Estimation. \emph{Journal of the American Statistical
#' Association}, 102(477), 359-378. (Defines the energy-form representation
#' of the CRPS used here, their eq. 21.)
#'
#' Zamo, M. and Naveau, P. (2018). Estimation of the Continuous Ranked
#' Probability Score with Limited Information and Applications to Ensemble
#' Weather Forecasts. \emph{Mathematical Geosciences}, 50(2), 209-234.
#' (Calls this the "NRG" estimator, dividing the pairwise term by \eqn{n^2}
#' rather than \eqn{n(n-1)}; see that paper for the alternative, less
#' biased "fair"/PWM estimator.)
#'
#' @export
crps_empirical <- function(y, pred_pdd) {
  # If the observation itself is missing, CRPS cannot be computed
  if (is.na(y)) {
    return(NA_real_)
  }

  # Drop missing draws; if none remain, CRPS cannot be computed
  x <- pred_pdd[!is.na(pred_pdd)]
  n <- length(x)
  if (n == 0L) {
    return(NA_real_)
  }

  # Sorting lets both terms below be computed in O(n) (after the
  # O(n log n) sort) instead of via an O(n^2) loop over all pairs
  x <- sort(x)

  # First term: (1/n) * sum_i |x_i - y|
  # Split the sorted draws into those <= y and those > y, so the
  # absolute value can be dropped and each half summed directly.
  idx_le <- x <= y
  m <- sum(idx_le)

  # Draws <= y: |x_i - y| = y - x_i, summed as y*m - sum(x_i)
  if (m > 0L) {
    A1 <- y * m - sum(x[idx_le])
  } else {
    A1 <- 0
  }

  # Draws > y: |x_i - y| = x_i - y, summed as sum(x_i) - y*(n-m)
  if (m < n) {
    A2 <- sum(x[!idx_le]) - y * (n - m)
  } else {
    A2 <- 0
  }

  # Combine and average over the n draws
  A <- (A1 + A2) / n

  # Second term: (1 / n^2) * sum_{i<j} |x_j - x_i|
  # For sorted x, |x_j - x_i| = x_j - x_i when i < j, so the double sum
  # over pairs reduces to a single weighted sum over the sorted draws:
  # the k-th smallest draw is the larger element in (k-1) pairs (+1
  # each) and the smaller element in (n-k) pairs (-1 each), giving net
  # weight (2k - n - 1).
  k <- seq_len(n)
  S_pair <- sum((2 * k - n - 1) * x)
  B <- S_pair / (n * n)

  # CRPS
  A - B
}
