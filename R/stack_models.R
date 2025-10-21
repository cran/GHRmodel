#' Combine (Stack) Multiple **GHRmodels** Objects
#'
#' @title Merge GHRmodels
#' @description
#' This function stack together two or more objects `GHRmodels` object,
#' returning **one** `GHRmodels` object that contains *all* the input models.
#'
#' If **any** `model_id` is duplicated across the inputs the `new_name` argument must be provided 
#' to ensure unique IDs.
#' @param ...   Two or more `GHRmodels` objects, or a single list of them.
#' @param new_name  `NULL` (default) **or** a character used to build the new
#'              model IDs.  
#' @param vs_first Logical. If TRUE columns comparing the model vs the first model
#'        are kept in the `mod_gof`, otherwise are discarded. Default is FALSE.
#'        Set to TRUE only when models contained in the `GHRmodels` object to be stacked
#'        are compared with the same first models.
#'
#' @return A single `GHRmodels` object containing all models from the inputs.
#'
#' @seealso
#' \code{\link{subset_models}} for subsetting GHRmodels objects, 
#' \code{\link{fit_models}} for fitting INLA models.
#' 
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#' 
#' # Load example GHRmodels object with DLNM from the package:
#' model_dlnm_file <- system.file("examples", "model_dlnm.rds", package = "GHRmodel")
#' model_dlnm <- readRDS(model_dlnm_file)
#'
#' # Merge models from the model_list and model_dlnm objects
#' model_stack <- stack_models( 
#'   model_list,
#'   model_dlnm, 
#'   new_name = "mod")
#'   
#' # The combined model_stack combines the models in the model_list and model_dlnm objects
#' model_stack$mod_gof$model_id  
#' }
#' 
#' 
#' @export
#' @rdname stack_models
#' 

stack_models <- function(..., new_name = NULL, vs_first = FALSE) {

  # 1) Checks
  
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) &&
      !inherits(dots[[1L]], "GHRmodels")) {
    dots <- dots[[1L]]}
  
  if (length(dots) < 2L){
    stop("At least two 'GHRmodels' objects are required.")
  }
  
  if (!all(vapply(dots, inherits, logical(1), what = "GHRmodels"))) {
    stop("Every input must be an object of class 'GHRmodels'.")
  }
  
  n_inputs <- length(dots)
  
  # 2) Extract per-slot lists
  mod_gof_list  <- lapply(dots, `[[`, "mod_gof")
  fitted_list   <- lapply(dots, `[[`, "fitted")
  fixed_list    <- lapply(dots, `[[`, "fixed")
  random_list   <- lapply(dots, `[[`, "random")
  vcov_list     <- lapply(dots, `[[`, "vcov")
  formulas_list <- lapply(dots, `[[`, "formulas")
  family_list   <- lapply(dots, `[[`, "family")
  
  # 3) Duplicate-ID handling 
  
  old_ids    <- unlist(lapply(mod_gof_list, `[[`, "model_id"), use.names = FALSE)
  duplicates <- anyDuplicated(old_ids)
  
  if (duplicates) {
    # Require 'new_name' 
    if (is.null(new_name))
      stop("Some 'model_id's overlap across inputs. ",
           "Provide the 'new_name' argument to generate new IDs.")
    
    # Validate 'new_name'
    if (length(new_name) == 1L) {
      # one prefix, all models share the same counter
      total_models <- length(old_ids)
      id_stream    <- paste0(new_name, seq_len(total_models))
      split_sizes  <- vapply(mod_gof_list, nrow, integer(1))
      new_ids_list <- split(id_stream,
                            rep(seq_along(split_sizes), split_sizes))
    } else {
      stop("'new_name' must be length 1")
    }
    
    # Replace IDs *everywhere*
    for (i in seq_len(n_inputs)) {
      ids_i <- new_ids_list[[i]]
      
      mod_gof_list[[i]]$model_id            <- ids_i
      names(formulas_list[[i]])             <- ids_i
      names(fitted_list[[i]])               <- ids_i
      names(fixed_list[[i]])                <- ids_i
      names(random_list[[i]])               <- ids_i
      names(vcov_list[[i]])                 <- ids_i
    }
  }
  
  # 4) Harmonise `$mod_gof` column sets 
  # (they can differ because models use different covariates)
  
  all_cols <- Reduce(union, lapply(mod_gof_list, names))
  
  # Remove columns if vs_first = FALSE
  if (!vs_first) {
    exclude_pattern <- "(r2dev|crpss|_vs_first_lci|_vs_first_uci|_vs_first)$"
    all_cols <- all_cols[!grepl(exclude_pattern, all_cols)]
  }
  
  mod_gof_list <- lapply(mod_gof_list, function(df) {
    df <- df[, intersect(all_cols, names(df)), drop = FALSE]
    missing <- setdiff(all_cols, names(df))
    if (length(missing)) df[missing] <- NA
    df[all_cols]
  })
  
  # 5) Assemble the stacked object 
  
  out <- list(
    mod_gof   = do.call(rbind, mod_gof_list),
    fitted    = do.call(c, fitted_list),
    fixed     = do.call(c, fixed_list),
    random    = do.call(c, random_list),
    vcov      = do.call(c, vcov_list),
    formulas  = unlist(formulas_list,  use.names = FALSE),
    family    = unlist(family_list,    use.names = FALSE),
    data      = dots[[1]]$data,
    re        = dots[[1]]$re,
    outcome   = dots[[1]]$outcome
  )
  
  class(out) <- c("GHRmodels", class(out))
  # 6) Return the output
  out
}




