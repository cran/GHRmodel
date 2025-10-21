#' Create a Two-Dimensional INLA-compatible Cross-basis Matrix
#'
#' This function is a wrapper around \link[dlnm:crossbasis]{dlnm::crossbasis} 
#' to generate cross-basis matrices that 
#' capture nonlinear effects of a predictor across both exposure and lag dimensions. 
#' The input covariate is passed as a numeric matrix of lagged values, and the resulting 
#' columns can be renamed via \code{basis_name} for easier reference in model formulas. 
#'
#' @param covariate A numeric matrix of covariate values. Typically this will be a 
#' matrix of lagged covariate values (which can be generated using \code{\link{lag_cov}}).
#' @param basis_name A character string specifying the prefix for the spline columns
#'   in the resulting basis matrix (replacing the default \code{"v"}).
#' @param lag A numeric vector with min and max lag of the matrix (as in \code{\link[dlnm]{crossbasis}}).
#' @param argvar A list specifying the shape of the exposure-response function
#'   (as in \code{\link[dlnm]{crossbasis}}).
#' @param arglag A list specifying the shape of the lag-response function
#'   (as in \code{\link[dlnm]{crossbasis}}).
#' @param ... Additional arguments passed to \link[dlnm:crossbasis]{dlnm::crossbasis}, such as
#'   \code{df}, \code{degree}, \code{knots}, etc.
#'
#' @return An object of class \code{"crossbasis_inla"} (also inheriting class \code{"crossbasis"}),
#'   as returned by `dlnm:crossbasis()` but with customized column names.
#'
#' @examples
#'
#' # Build cross-basis with a custom prefix for columns
#' 
#' # Import example data set 
#' data("dengue_MS")
#' 
#' lag_mat <- lag_cov(data = dengue_MS,
#'   name = c("tmin"),
#'   time = "date",
#'   lag = c(1:6),
#'   group = "micro_code",
#'   add = FALSE) # add = FALSE return only the lagged matrix
#'   
#' cb_inla <- crossbasis_inla(
#'   covariate  = lag_mat,
#'   basis_name = "tempLag",
#'   lag = c(1,6),
#'   argvar = list(fun = "bs", df = 3),
#'   arglag = list(fun = "poly", degree = 2)
#' )
#'
#'# Check class of the cross-basis object
#' class(cb_inla)
#' 
#' # View resulting cross-basis matrix
#' head(colnames(cb_inla))
#'
#' @export

crossbasis_inla <- function(
    covariate, 
    basis_name, 
    lag, 
    argvar = list(), 
    arglag = list(),
    ...) {
  
  # Checks 
  if (missing(covariate)) {
    stop("'covariate' is required but missing.")
  }
  if (!is.matrix(covariate) && !is.data.frame(covariate)) {
    stop("'covariate' must be a matrix or data.frame.")
  }
  if (!is.numeric(as.matrix(covariate))) {
    stop("'covariate' must contain numeric values.")
  }
  if (missing(basis_name)) {
    stop("'basis_name' is required but missing.")
  }
  if (!is.character(basis_name) || length(basis_name) != 1 || nchar(basis_name) == 0) {
    stop("'basis_name' must be a non-empty character string.")
  }
  if (missing(lag)) {
    stop("'lag' is required but missing.")
  }
  if (!is.list(argvar) || length(argvar) == 0) {
    stop("'argvar' must be a non-empty list.")
  }
  if (!is.list(arglag) || length(arglag) == 0) {
    stop("'arglag' must be a non-empty list.")
  }
  
  # Call crossbasis from dlnm
  cb <- dlnm::crossbasis(
    x       = covariate,
    lag     = lag,
    argvar  = argvar,
    arglag  = arglag,
    ...
  )
  
  # Rename columns (replace the default 'v' with 'basis_name')
  old_names <- colnames(cb)
  new_names <- sub("^v", basis_name, old_names)
  colnames(cb) <- new_names
  
  # Append an additional class
  class(cb) <- append("crossbasis_inla", class(cb))
  
  return(cb)
}
