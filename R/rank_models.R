#' @title Rank Models by Goodness-of-Fit
#'
#' @description
#' This function ranks fitted models in a `GHRmodels` object by a chosen metric
#' (e.g., `dic`, `waic`, `crps`, etc.).
#'
#' @param models A `GHRmodels` object containing fitted model output.
#' @param metric A character string indicating which goodness-of-fit metric to use
#'   for ranking. One of: `"dic"`, `"waic"`,`"lms"`,`"mae"`, `"rmse"`,`"crps"`, 
#'   `"rsq"`,`"dic_vs_first"`, `"waic_vs_first"`, `"mae_vs_first"`, `"rmse_vs_first"`, 
#'   `"crps_vs_first"`, `"re_n_var"`, and `"re_n_var_change"` (where n is the 
#'   number of random effect, for ex. `re_1_var`, `re_1_var_change`).
#' @param n An integer specifying how many top-ranked models to return (default `10`).
#'
#' @return
#' A character vector of the top model IDs (in ascending order of the specified `metric`).
#'
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Get a list of the 5 best models by DIC
#' top_model_dic <- rank_models(
#'   models = model_list,
#'   metric = "dic",
#'   n = 5
#' )
#' top_model_dic
#' }
#' @seealso 
#' \code{\link{fit_models}} for fitting multiple INLA models.
#' @name rank_models
#' @rdname rank_models
#' @export

rank_models <- function(models,
                        metric = "dic",
                        n = 10) {
  
  # Check object 
  
  if (!inherits(models, "GHRmodels"))
    stop("`model` must be of class 'GHRmodels'.", call. = FALSE)
  
  if(metric %in% c("r2dev","crpss")){
    decreasing <- TRUE
  } else {
    decreasing <- FALSE
  }
  
  if(isTRUE(decreasing)){
    ranked_models <- models$mod_gof |> 
      dplyr::arrange(desc(.data[[metric]])) |> 
      dplyr::slice_head(n = n)
  }else if(!isTRUE(decreasing)){
    ranked_models <- models$mod_gof |> 
      dplyr::arrange(.data[[metric]]) |> 
      dplyr::slice_head(n = n)
  }
  
  # Handle the case where `n` exceeds the number of available models
  total_models <- nrow(models$mod_gof)
  if (n > total_models) {
    warning(sprintf("'n' (%d) exceeds the total number of models (%d). Returning all available models.", n, total_models))
    ranked_models <- models$mod_gof |> 
      dplyr::arrange(.data[[metric]]) |> 
      dplyr::slice_head(n = total_models)
  }
  
  # Extract the top model IDs
  top_model_ids <- ranked_models$model_id
  
  return(top_model_ids)
}
