# plot_coef_lin -----------------------------------------------------------
#' Produce a Forest Plot of Linear Covariates from a \code{GHRmodels} Object
#'
#' This function extracts fixed-effect coefficients from a specified model in \code{models},
#' filters them by name or interaction pattern, and produces a forest plot (point estimates
#' with error bars). 
#' - If \code{name = NULL}, all fixed-effect terms (excluding the intercept) are shown.  
#' - If \code{name} is a character vector, only the matching terms are included.  
#'
#' @param models An object of class \code{GHRmodels} containing fitted model output.
#' @param mod_id Character vector of model identifiers (must match entries in `model$mod_gof$model_id`).
#' If \code{NULL} (the default), all models are considered.
#' @param name A character vector specifying exact linear covariates names to be plotted. 
#' If both \code{pattern} and \code{name} are \code{NULL} (the default), all 
#' terms (except \code{(Intercept)}) are plotted.
#' @param pattern  A character vector specifying prefix(es) to match (e.g., "tmin" matches "tmin", "tmin.l1", etc.)
#' Covariates matching these patterns (case‐insensitive search) will 
#' be plotted. If both \code{pattern} and \code{name} are
#' \code{NULL} (the default), all terms (except \code{(Intercept)}) are plotted.
#' @param title An optional string specifying an overall plot title.
#' @param mod_label An optional named character vector mapping model names
#' to custom labels, e.g.  c("mod1" = "Model 1"). Any model not found in the
#' vector names retains its original label. 
#' @param var_label An optional named character vector mapping variable (or interaction) names
#' to custom labels. Interaction matching is order-insensitive: \code{"A:B"} matches
#' \code{"B:A"}. Any term not found in the vector names retains its original label.
#' @param palette GHR, RColorBrewer or colorspace palette (e.g. "Purp") colour 
#' palette to use for the different models. See all available options by running 
#' `GHR_palettes()`, `RColorBrewer::display.brewer.all()` and 
#' `colorspace::hcl_palettes(plot=TRUE)`. Single R colors in `colors()` or hex 
#' codes can also be used.
#' @param exp Logical,if \code{TRUE} the coefficients are exponentiated, Default is if \code{FALSE}.
#' @param legend Legend title for the replicate color scale. Default is \code{"Model"}.
#'
#' @return A \pkg{ggplot2} forest plot object (class \code{ggplot}).
#'
#' @details
#' \describe{
#'   \item{Intercept}{By default, \code{(Intercept)} is excluded unless explicitly included in
#'         \code{name}.}
#'   \item{Individual terms}{e.g., \code{"temp"}.}
#'   \item{Interaction Terms}{e.g. \code{"temp:precip"}. Split by \code{:}, sorted, 
#'   and compared setwise; for example, \code{"temp:precip"} matches \code{"precip:temp"}.}
#'   \item{Labels}{If \code{var_label} is supplied, any matched covariate or interaction
#'         string is replaced by its custom label on the y-axis.}
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_pointrange}} for the plotting environment.
#' @importFrom stats setNames median
#'
#' @examples
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Plot point estimates with confidence intervals for the linear covariates: 
#' plot_coef_lin(
#' model = model_list,
#' mod_id = c("mod2","mod4"),
#' var_label = c("tmin.l1"= "Min. temp lag 1",
#'               "pdsi.l1" = "Drought index lag 1"),
#' title = "Effects of linear covariates"
#' )
#'
#'
#' @export

plot_coef_lin <- function(models,
                          mod_id = NULL,
                          name = NULL,
                          pattern = NULL,
                          title = NULL,
                          mod_label = NULL,
                          var_label = NULL,
                          palette = "IDE2",
                          exp = FALSE,
                          legend = "Model") {
  
  # 0) Check models object 
  if (!inherits(models, "GHRmodels")) {
    stop("'models' must be an object of class 'GHRmodels'.")
  }
  
  # 1) Check mod_id validity. If mod_id is NULL, consider all models
  if (is.null(mod_id)) {
    mod_id <- models$mod_gof[["model_id"]]
  } else {
    if (!all(mod_id %in% models$mod_gof[["model_id"]])) {
      stop("One of the 'mod_id' not found in 'GHRmodels' object.")
    }
  }
  
  if (is.null(names(mod_label)) && !is.null(mod_label)) {
    stop("'mod_label' must be a named vector")
  }
  
  # 2) Extract the fixed-effect table for the chosen models
  fixef_df <- data.frame()
  for (m_id in mod_id) {
    fixef_id <- as.data.frame(models$fixed[[m_id]])
    fixef_id$varname <- row.names(fixef_id)
    row.names(fixef_id) <- NULL
    fixef_id$model <- m_id
    fixef_df <- rbind(fixef_df, fixef_id)
  }
  
  # 3) If user doesn't specify name or pattern, plot all available coefficients 
  # except the Intercept
  var_all <- c()
  all_terms <- fixef_df$varname
  if (is.null(name) & is.null(pattern)) {
    var_all <- setdiff(all_terms, "(Intercept)")
  }
  
  # 4) Exact names
  if (!is.null(name)) {
    if (!is.vector(name)) {
      stop("'name' must be a vector of strings.")
    }
    var_all <- name
  }
  
  # 5) Regex patterns
  if (!is.null(pattern)) {
    if (!is.vector(pattern)) {
      stop("'pattern' must be a vector of strings.")
    }
    for (pat in pattern) {
      matches <- grep(pat, all_terms, value = TRUE, ignore.case = TRUE)
      if (length(matches) > 0) {
        var_all <- c(var_all, matches)
      }
    }
    var_all <- unique(var_all)
  }
  
  # Helper: rearrange interaction terms in alphabetical order
  arrange_int <- function(term) {
    parts <- unlist(strsplit(term, ":"))
    paste(sort(parts), collapse = ":")
  }
  
  match_interaction <- function(row_term, target_term) {
    setequal(
      unlist(strsplit(row_term, ":")),
      unlist(strsplit(target_term, ":"))
    )
  }
  
  # 6) Match rows for plotting
  row_index <- integer(0)
  for (this_var in var_all) {
    idx <- which(sapply(all_terms, match_interaction, this_var))
    if (length(idx) > 0) {
      row_index <- c(row_index, idx)
    }
  }
  row_index <- unique(row_index)
  
  if (length(row_index) == 0) {
    warning(sprintf("No linear effects matched by name/pattern found in model%s '%s'.",
                    if (length(mod_id) > 1) "s" else "",
                    paste(mod_id, collapse = ", ")),
            call. = FALSE)
    return(NULL)
  }
  
  
  # 7) Build plot data
  df_plot <- data.frame(
    variable = fixef_df$varname[row_index],
    model    = fixef_df$model[row_index],
    mean     = fixef_df$mean[row_index],
    lci      = fixef_df$`0.025quant`[row_index],
    uci      = fixef_df$`0.975quant`[row_index],
    stringsAsFactors = FALSE
  )
  
  # 8A) Apply variable labels (optional)
  if (!is.null(var_label)) {

    # Precompute arranged forms for each key in var_label
    var_label_arranged <- sapply(names(var_label), arrange_int)
    df_plot$final_label <- sapply(df_plot$variable, function(rv) {
      rv_arranged <- arrange_int(rv)
      match_idx <- which(var_label_arranged == rv_arranged)
      if (length(match_idx) > 0) {
        var_label[[ names(match_idx)[1] ]]
      } else {
        rv
      }
    })
  } else {
    df_plot$final_label <- df_plot$variable
  }
  
  # Reverse order of variable names for y-axis
  df_plot$final_label <- factor(df_plot$final_label,
                                levels = unique(rev(df_plot$final_label)))
  
  # 8B) Apply model labels (optional) and enforce user order
  if (is.null(mod_label)) mod_label <- setNames(mod_id, mod_id)
  
  df_plot$model_label <- sapply(df_plot$model, function(x) {
    match_idx <- which(names(mod_label) == x)
    if (length(match_idx) > 0) {
      mod_label[match_idx]
    } else {
      x
    }
  })
  
  # Enforce factor levels in input order
  df_plot$model_label <- factor(df_plot$model_label,
                                levels = unname(mod_label[mod_id]))
  
  # 9) Color palette
  if (length(unique(df_plot$model)) == 1) {
    my_palette <- rev(GHRexplore::GHR_palette(palette, 2)(2))[1]
  } else {
    my_palette <- rev(GHRexplore::GHR_palette(palette, length(unique(df_plot$model)))(
      length(unique(df_plot$model))
    ))
  }
  
  # 10) Plot
  p <- ggplot2::ggplot(df_plot) +
    ggplot2::geom_vline(xintercept = 0, color = "grey70", linetype = "dashed") +
    ggplot2::geom_pointrange(
      ggplot2::aes(x = mean, xmin = lci, xmax = uci, 
                   y = final_label, color = model_label),
      position = position_dodge(width = 0.4, preserve = "total")) + 
    ggplot2::scale_color_manual(values = my_palette, name = legend) + 
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = "Effect size (95% CI)", y = NULL)
  
  if (exp) {
    p <- p + ggplot2::scale_x_continuous(transform = .exp_trans,
                                         labels = function(x) round(exp(x), 2)
    )
  } 
  
  # 10) Optionally add a plot title, delete legend if only one model
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  
  if (length(unique(df_plot$model)) == 1) {
    p <- p + ggplot2::guides(colour = "none")
  }
  
  return(p)
}

# plot_coef_nl -----------------------------------------------------------
#' Plot Nonlinear Effects from a \code{GHRmodels} Object
#'
#' Generates plots of nonlinear effects from one or more fitted models contained within a `GHRmodels` object.
#' The function supports two main display modes: 
#' - Grid (when `collapse = FALSE`): one plot per covariate and model, with effects by column and models by row.
#' \itemize{
#'   \item If multiple models are specified, the user must provide either \code{name} or \code{pattern}
#'   to select which nonlinear effects to plot.
#'   \item If only one model is selected and both \code{name} and \code{pattern} are \code{NULL},
#'   all nonlinear effects in the model will be plotted.
#' }
#'- Collapsed (when `collapse = TRUE`): one non-linear effect combined across models into a single panel.
#' \itemize{
#'   \item The user must explicitly specify the exact variable name using \code{name}.
#'   It only accepts one covariate name.
#'   \item Collapse mode can only be used when the selected effect is not replicated 
#'   (that is, does not have the format `f(covariate, model = ..., replicate = group))`
#'   If replication is detected, an error will be thrown.
#' }
#'
#' @param models A \code{GHRmodels} object containing fitted model outputs.
#' @param mod_id Integer vector specifying which model(s) to plot (as indexed in \code{model$models}).
#' @param mod_label An optional named character vector mapping model names
#' to custom labels, e.g.  c("mod1" = "Model 1"). Any model not found in the
#' vector names retains its original label. 
#' @param name Optional character vector of variable names (as used in \code{inla.group(...)})
#'   to select specific nonlinear effects. Required for collapse mode.
#' @param pattern Optional regular expression pattern to match effect names.
#'   Used to select nonlinear effects when \code{name} is not provided.
#' @param title Optional overall title for the plot.
#' @param var_label Optional named character vector providing custom labels for each nonlinear variable.
#'   Names must match the variable names (e.g., used in \code{inla.group(x)}), not full effect names.
#' @param palette Name of the color palette to use (passed to \code{GHR_palette}). Default is \code{"IDE2"}.
#' @param xlim Optional named list specifying x-axis limits for each effect.
#'   Each element should be a numeric vector of length 2: \code{list(var1 = c(min, max), var2 = c(min, max))}.
#'   Variable names must match those used in \code{inla.group()}.
#' @param ylab Optional y-axis label. If \code{NULL}, defaults to \code{"Effect size"}.
#' @param xlab Optional x-axis label. If \code{NULL}, defaults to \code{"<variable> values"}.
#'   If explicitly set to \code{NULL}, no x-axis label will be shown.
#' @param histogram Logical; if \code{TRUE} (default), includes a histogram below each partial-effect plot.
#' @param legend Legend title for the replicate color scale (if multi-replicate effects are present). Default is \code{"Replicate"}.
#' @param hist_fill Fill color for histogram bars. Default is \code{"grey"}.
#' @param rug Include a rug plot in the x-axis. Default is FALSE.
#' @param collapse Logical; if \code{TRUE}, attempts to collapse plots across models to show one plot per variable.
#' This requires that selected nonlinear effect is not replicated (i.e. the covariate
#'  is not in the format f(covariate, model = ..., replicate = group))
#' @param exp Logical,if \code{TRUE} the coefficients are exponentiated, Default is if \code{FALSE}.
#' @return A \code{ggplot} or \code{cowplot} object, depending on the plotting mode.
#' 
#' @importFrom stats setNames median
#' 
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Plot 2 models with non-linear PDSI at one month lag in collapsed mode: 
#' plot_coef_nl(
#'   models = model_list,
#'   mod_id = c( "mod5", "mod6") ,
#'   mod_label = c("mod6" = "pdsi.l1_nl",
#'                 "mod5" = "pdsi.l1_nl + tmin.l1_nl"),
#'   var_label = c("pdsi.l1" = "Drought index (PDSI)"),
#'   name = c("pdsi.l1"),
#'   title = "Change in PDSI with and without mean min. temp lag 1",
#'   xlab = "PDSI",
#'   palette = "IDE2",
#'   collapse = TRUE    
#' )
#' }
#'
#' @export

plot_coef_nl <- function(models,
                         mod_id,
                         mod_label   = NULL,
                         name        = NULL,
                         pattern     = NULL,
                         title       = NULL,
                         var_label   = NULL,
                         palette     = "IDE2",
                         xlim        = NULL,
                         ylab        = NULL,
                         xlab        = NULL,
                         histogram   = FALSE,
                         legend      = NULL,
                         hist_fill   = "grey",
                         rug        = FALSE,
                         collapse    = FALSE,
                         exp = FALSE) {
  
  # 0) Check models object 
  if (!inherits(models, "GHRmodels")) {
    stop("'models' must be an object of class 'GHRmodels'.")
  }
  
  # Decide which variables to select
  all_effects_by_model <- lapply(mod_id, function(m) {
    grep("inla\\.group", names(models$random[[m]]), value = TRUE)
  })
  all_unique_effects <- unique(unlist(all_effects_by_model))
  
  selected_nl_names <- character(0)
  if (!is.null(name)) {
    for (v in name) {
      pattern_v <- paste0("^INLA::inla\\.group\\(", v, "(\\)|,)")
      matched <- grep(pattern_v, all_unique_effects, value = TRUE)
      if (length(matched) == 0) {
        warning(sprintf("Variable '%s' not found in any models", v))
        return(NULL)
      }
      selected_nl_names <- c(selected_nl_names, matched)
    }
  }
  if (!is.null(pattern)) {
    matched_pattern <- grep(pattern, all_unique_effects, value = TRUE)
    if (length(matched_pattern) == 0) {
      warning(sprintf("Pattern '%s' not found in any models", pattern))
      return(NULL)
    }
    selected_nl_names <- c(selected_nl_names, matched_pattern)
  }
  selected_nl_names <- unique(selected_nl_names)
  
  # Determine whether any of the selected effects are replicated
  is_replicated <- FALSE
  for (m in mod_id) {
    for (nl in selected_nl_names) {
      if (nl %in% names(models$random[[m]])) {
        df0 <- models$random[[m]][[nl]]
        if (ncol(df0) < 8) next
        colnames(df0)[1:8] <- c("ID", "mean", "sd", "lci", "est", "uci", "mode", "kld")
        df0$ID <- as.numeric(as.character(df0$ID))
        n_unique <- length(unique(df0$ID))
        n_rep <- nrow(df0) / n_unique
        if (n_rep > 1) {
          is_replicated <- TRUE
          break
        }
      }
    }
    if (is_replicated) break
  }
  
  if (collapse) {
    if (is_replicated) {
      stop("Cannot collapse nonlinear effects of multiple models when one of the effects is replicated.")
    }
    result <- suppressWarnings(.plot_coef_nl_collapse(model      = models,
                                  mod_id     = mod_id,
                                  mod_label  = mod_label,
                                  name        = name,
                                  title      = title,
                                  var_label  = var_label,
                                  palette    = palette,
                                  xlim       = xlim,
                                  ylab       = ylab,
                                  xlab       = xlab,
                                  histogram  = histogram,
                                  legend     = legend,
                                  hist_fill  = hist_fill,
                                  rug        = rug,
                                  exp        = exp)
    )
  } else {
    result <- suppressWarnings(.plot_coef_nl_grid(model      = models,
                              mod_id     = mod_id,
                              mod_label  = mod_label,
                              name       = name,
                              pattern    = pattern,
                              title      = title,
                              var_label  = var_label,
                              palette    = palette,
                              xlim       = xlim,
                              ylab       = ylab,
                              xlab       = xlab,
                              histogram  = histogram,
                              legend     = legend,
                              hist_fill  = hist_fill,
                              rug        = rug,
                              exp        = exp)
                              )
  }
  if (is.null(result)) {
    warning(sprintf("No nonlinear effects found in model%s '%s'.",
                    if (length(mod_id) > 1) "s" else "",
                    paste(mod_id, collapse = ", ")))
    return(NULL)
  } else {
    return(result)
  }
}



# plot_coef_varying -----------------------------------------------------------
#' Produce a Forest Plot for a Spatially or Temporally Varying Effects from a `GHRmodels` object.
#'
#' @description
#' Generates a forest plot for a specified spatially or temporally varying coefficient 
#' (i.e. a random slope) from a fitted `GHRmodels` object. The plot displays the
#'  effect estimates (x-axis) for each spatial/temporal unit (y-axis).
#'
#' @param models A `GHRmodels` object containing fitted model output.
#' @param mod_id A character specifying which model to be plotted (as in `models$mod_gof$model_id`).
#' @param name A character string naming the spatially or temporally varying coefficient to plot.
#'   This should match a random effect name in `models$random[[mod_id]]`.
#' @param unit_label Optional named character vector providing custom labels for each spatial/temporal unit.
#' @param title Optional string for the plot title.
#' @param palette Character string for the GHR, RColorBrewer or colorspace palette (e.g. "Purp") colour 
#' palette to use for the different models. See all available options by running 
#' `GHR_palettes()`, `RColorBrewer::display.brewer.all()` and 
#' `colorspace::hcl_palettes(plot=TRUE)`. Single R colors in `colors()` or hex 
#' codes can also be used.
#' @param ylab Optional character string for the y-axis label (default constructed from varying covariate name).
#' @param xlab Optional character string for the x-axis label (default = "Effect size").
#' @param exp Logical,if \code{TRUE} the coefficients are exponentiated, Default is if \code{FALSE}.
#' 
#' @return A `ggplot2` forest plot object representing the spatially or temporally varying effect,
#'   with each line corresponding to a different spatial or temporal unit.
#'
#' @examples
#' \donttest{
#' 
#' # Load example GHRmodels object from the package: 
#' model_cov_list_file <- system.file("examples", "model_cov_list.rds", package = "GHRmodel")
#' model_cov_list <- readRDS(model_cov_list_file)
#' 
# Plot varying slopes of pdsi.l1 depending on the climate zone
#' plot_coef_varying(
#'   models = model_cov_list,               # A list of fitted INLA model objects
#'   mod_id = "mod8",                       # Select the model with varying slopes
#'   palette = "Blues",                     # Color palette for the plot 
#'   name = "main_climate_f",               # The grouping variable 
#'   title = "Effect of PDSI at one-month lag for each climate zone",  # Plot title
#'   ylab = "Main climate zones",           # Label for the y-axis 
#'   unit_label = c(                        # Map factor levels to descriptive names 
#'     "1" = "Tropical Rainforest Climate", 
#'     "2" = "Tropical Monsoon Climate", 
#'     "3" = "Tropical Savanna Climate with Dry Winter",
#'     "4" = "Humid Subtropical Climate")
#' )
#' }
#' 
#' @export

plot_coef_varying <- function(models,
                              mod_id,
                              name,
                              unit_label  = NULL,  
                              palette    = "IDE2",
                              title      = NULL,
                              xlab = "Effect size",
                              ylab = NULL,
                              exp = FALSE) {
  
  #  1) Basic checks 
  if (is.null(mod_id)) {
    stop("'mod_id' must be provided.")
  }
  
  if (length(mod_id) != 1) {
    stop("plot_coef_varying() accepts only one model at a time. Please provide a single 'mod_id'.")
  }
  
  if (!mod_id %in% models$mod_gof[["model_id"]]) {
    stop("'mod_id' not found in 'GHRmodels' object.")
  }
  
  if (length(name) != 1) {
    stop("Please supply exactly one variable name in 'name'.")
  }
  
  # 2) Locate the random-effect table matching `name` 
  re_names <- names(models$random[[mod_id]])
  idx <- grep(name, re_names)
  
  if (length(idx) == 0) {
    warning(sprintf("No varying effects term matching '%s' in model '%s'.",
                    name,
                    mod_id),
            call. = FALSE)
    return(NULL)
  }
  
  df_est <- models$random[[mod_id]][[idx[1]]]
  names(df_est)[1:8] <- c("ID", "mean", "sd", "lci", "est", "uci", "mode", "kld")
  df_est$ID <- as.numeric(as.character(df_est$ID))
  
  # 3) Order by numeric ID 
  df_est$ID <- factor(df_est$ID, levels = sort(unique(df_est$ID)))
  df_est <- dplyr::arrange(df_est, ID)
  
  # 4) Apply unit_label (custom unit labels) if provided
  if (!is.null(unit_label)) {
    if (is.null(names(unit_label))) {
      stop("'unit_label' must be a named vector")
    }
    
    # Get factor levels (original numeric IDs)
    id_levels <- as.character(levels(df_est$ID))
    
    # Check that all IDs have a label
    if (!all(id_levels %in% names(unit_label))) {
      missing_ids <- id_levels[!(id_levels %in% names(unit_label))]
      stop(sprintf("Missing labels for the following IDs: %s", paste(missing_ids, collapse = ", ")))
    }
    
    # Apply new labels
    levels(df_est$ID) <- unit_label[id_levels]
  }
  
  
  # 5) Automatic x label and title if they are null
  
  if(is.null(ylab)){ 
    ylab <- name
  }
  
  
  if(is.null(title)) {
    identified_row <- models$mod_gof[models$mod_gof$model_id == mod_id,]
    identified_covariate <- identified_row[!is.na(identified_row) & grepl(paste0("f\\(",name), identified_row)]
    title <- sub("^[^,]*,\\s*([^,]*).*", "\\1", identified_covariate)
  }
  
  # 6) Build the forest plot
  color <- rev(GHRexplore::GHR_palette(palette, 2)(2))[1]
  
  p <- ggplot2::ggplot(df_est) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(x = mean, y = ID, xmin = lci, xmax = uci), 
                             colour = color) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_bw(base_size = 12)
  
  if (exp) {
    p <- p + ggplot2::scale_x_continuous(transform = .exp_trans,
                                         labels = function(x) round(exp(x), 2)
    )
  } 
  
  
  return(p)
}

