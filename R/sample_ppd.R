#' @title Sample from the Posterior Predictive Distribution
#' 
#' @description
#' This function refits a specified model from a `GHRmodels` object and generates
#'  samples from its posterior predictive distribution.
#'
#' @param models A \code{GHRmodels} object.
#' @param mod_id Character; model identifier (from \code{models$mod_gof$model_id}).
#' @param s An integer specifying the number of samples to draw from the posterior predictive distribution.
#' @param nthreads An integer specifying the number of threads for parallel computation to refit the model. 
#' Default is \code{8}.
#'
#' @return A \code{data.frame} containing columns for each of the posterior 
#' predictive samples and one column with observed data.
#' 
#' @rdname sample_ppd
#' @name sample_ppd
#' @examples
#' \donttest{
#' # Load example dataset
#' data(dengueMS)
#' 
#' # Declare formulas
#' formulas <- c("dengue_cases ~ tmin +  f(year, model='rw1')")
#' 
#' # Tranform formulas into a 'GHRformulas' object
#' ghr_formula <- as_GHRformulas(formulas)
#' 
#' # Fit multiple models 
#' results <- fit_models(
#'   formulas = ghr_formula,
#'   data     = dengue_MS[dengue_MS$year %in% 2005:2010,],
#'   family   = "nbinomial",
#'   name     = "model",
#'   offset   = "population",
#'   nthreads = 2,
#'   control_compute = list(config = FALSE),
#'   pb       = TRUE
#' )
#' 
#' # Generate 100 samples from the posterior predictive distribution of the model
#' ppd_df <- sample_ppd( 
#'   results,
#'   mod_id = "model1", 
#'   s = 100,
#'   nthreads = 2)
#' }
#'
#' @export

sample_ppd <- function(models,
                       mod_id,
                       s = 1000, 
                       nthreads = 8) {
  
  
  # Check mod_id exists
  if (!any(mod_id == models$mod_gof[["model_id"]])) {
    stop("'mod_id' not found in GHRmodels object")
  }
  
  index <- which(models$mod_gof[["model_id"]] == mod_id)
  
  distribution <- models$family[index]
  
  # Re-fit the selected model to extract ppd
  fitted_model <- .fit_single_model(
    formula    = models$formulas[index],
    family     = distribution,
    offset     = "offset",
    data       = models$data,
    control_compute = list(config = TRUE),
    nthreads   = nthreads
  )
  
  # Generate posterior predictions according to the family
  if (distribution == "nbinomial") {
    ppd <- .ppd_nb(fitted_model, s = s)
  } else if (distribution == "poisson") {
    ppd <- .ppd_pois(fitted_model, s = s)
  } else {
    stop("Unsupported family: ", distribution)
  }
  
  out <- as.data.frame(ppd)
  
  names(out)<- paste0("s_", 1:ncol(ppd))
  
  out$observed <- models$data[[models$outcome]]
  
  return(out)

}