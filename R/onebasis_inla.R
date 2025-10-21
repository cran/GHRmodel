#' Create a One-Dimensional Basis for INLA
#'
#' This function is a wrapper around \link[dlnm]{onebasis} to
#' create a one-dimensional basis for spline modeling. This wrapper enhances the
#'  original function by allowing users to specify a custom prefix for the column 
#'  names using the `basis_name` argument, such that each set of basis variables can 
#'  be easily identified in the model formula by the \pkg{INLA} framework.
#'
#' @param covariate A numeric vector representing the covariate
#' @param fun A character string specifying the shape function to be used by
#'   \code{\link[dlnm]{onebasis}}.
#' @param basis_name A character string giving a base name for the columns in the
#'   resulting basis matrix. The default prefix (usually \code{"b"}) is replaced by this string.
#' @param ... Additional arguments passed to \code{\link[dlnm]{onebasis}}, such as
#'   \code{degree}, \code{df}, \code{knots}, etc.
#'
#' @return An object of class \code{"onebasis"}, as returned by \code{\link[dlnm]{onebasis}},
#'   with column names modified according to \code{basis_name}.
#'
#' @examples
#' 
#' # Import example data set
#' data("dengue_MS")
#'
#' # Build a one-dimensional spline basis with a custom name
#' ob_inla <- onebasis_inla(
#'  covariate = dengue_MS$tmin,
#'  fun = "bs",
#'  basis_name = "tempBasis",
#'  degree = 2
#' )
#'
#' # Check class of the one-basis object
#' class(ob_inla)
#' 
#' # View first rows of the one-basis matrix
#' head(ob_inla)
#'
#' @export

onebasis_inla <- function(covariate, fun, basis_name, ...) {
  
  # Input validation
  if (missing(covariate)) {
    stop("'covariate' is required but missing.")
  }
  if (!is.numeric(covariate) || !is.vector(covariate)) {
    stop("'covariate' must be a numeric vector.")
  }
  
  if (missing(fun)) {
    stop("'fun' is required but missing.")
  }
  if (!is.character(fun) || length(fun) != 1 || nchar(fun) == 0) {
    stop("'fun' must be a non-empty character string.")
  }
  
  if (missing(basis_name)) {
    stop("'basis_name' is required but missing.")
  }
  if (!is.character(basis_name) || length(basis_name) != 1 || nchar(basis_name) == 0) {
    stop("'basis_name' must be a non-empty character string.")
  }
  
  # Call onebasis function from dlnm
  ob <- dlnm::onebasis(x = covariate, fun = fun, ...)
  
  # Rename columns (replace the default 'b' with 'basis_name')
  old_names <- colnames(ob)
  new_names <- sub("^b", basis_name, old_names)  
  colnames(ob) <- new_names
  
  return(ob)
}