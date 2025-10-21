#_______________________________________________________________________________
# Helper Functions for Plotting Model Output ----------------------------------
#_______________________________________________________________________________

# plotting_variable -----------------------------------------------------------

#' @title Format Plotting Variable
#'
#' @description
#' Internal helper function to prepare a variable for plotting. If the input is a character
#' vector or factor, it replaces `NA` values with `"none"` and returns a factor. Other types 
#' (numeric, logical, etc.) are returned unchanged.
#'
#' @param vec A vector to be formatted for plotting. 
#' Can be character, factor, numeric, logical, etc.
#'
#' @return
#' A formatted vector suitable for use in a plot. Character and factor vectors are returned
#' as factors with `NA`s replaced by `"none"`. Other types are returned unchanged.
#'
#' @noRd
#' @noRd


.plotting_variable <- function(vec) {
  if (is.character(vec) || is.factor(vec)) {
    vec <- as.character(vec)
    vec[is.na(vec)] <- "none"
    return(factor(vec))
  } else {
    return(vec)  # return numeric, logical, etc. unchanged
  }
}


# get_metric_label -----------------------------------------------------------

#' @title Get Custom Y-Axis Label for a Given Metric
#' @description
#' This function returns a custom y-axis label for a given metric used in the plot. 
#' The function handles various metrics, including model comparison metrics (e.g., DIC, WAIC, etc.)
#' and random effect-related metrics. If confidence intervals (`ci`) are provided, the label 
#' will include additional information about the credible intervals for certain metrics.
#'
#' @param metric A character string representing the metric for which the y-axis label is needed.
#'   Supported metrics include:
#'   - `"dic"`, `"waic"`, `"lms"`, `"mae"`, `"rmse"`, `"crps"`, `"rsq"`,
#'   - `"dic_vs_first"`, `"waic_vs_first"`, `"mae_vs_first"`, `"rmse_vs_first"`,
#'   - `"crps_vs_first"`, and random effect metrics like `"re_n_var"` and `"re_n_var_change"`,
#'   - where `n` represents the random effect index, e.g., `"re_1_var"`, `"re_2_var_change"`.
#' @param ci A logical value (`TRUE` or `FALSE`) indicating whether to include information about 
#'   the 95% credible interval for the difference. If `TRUE` and if the `metric` is `"dic_vs_first"` 
#'   or `"waic_vs_first"`, the label will append `"\n with 95% credible interval"`.
#'
#' @return A character string representing the custom y-axis label for the provided metric.
#' 
#' @noRd
#' 
.get_metric_label <- function(metric, ci = FALSE) {
  # Default metric labels
  metric_labels <- c(
    dic = "Deviance Information Criterion (DIC)",
    waic = "Watanabe-Akaike Information Criterion (WAIC)",
    lms = "Least Mean Squares (LMS)",
    mae = "Mean Absolute Error (MAE)",
    rmse = "Root Mean Squared Error (RMSE)",
    crps = "Continuous Ranked Probability Score (CRPS)",
    rsq = "R-squared (R^2)",
    dic_vs_first = "Difference in DIC vs. reference model",
    waic_vs_first = "Difference in WAIC vs. reference model",
    mae_vs_first = "Difference in MAE vs. reference model",
    rmse_vs_first = "Difference in RMSE vs. reference model",
    crps_vs_first = "Difference in CRPS vs. reference model"
  )
  
  # Check if ci is TRUE and if the metric is dic_vs_first or waic_vs_first
  if (ci && (metric == "dic_vs_first" || metric == "waic_vs_first")) {
    label <- paste0(metric_labels[[metric]], "\n with 95% credible interval")
  } else {
    # Handle the special cases for random effect variables
    if (grepl("^re_\\d+_var$", metric)) {
      label <- "Random Effect Variance"
    } else if (grepl("^re_\\d+_var_change$", metric)) {
      label <- "Proportion change of the random effect variance \nvs. reference model"
    } else if (metric %in% names(metric_labels)) {
      label <- metric_labels[[metric]]
    } else {
      label <- metric  # return the original metric name if no match is found
    }
  }
  
  return(label)
}

# get_metric_title -----------------------------------------------------------
#' @title Get Custom Title for a Given Metric
#' @description
#' This function returns a custom y-axis label for a given metric used in the plot. 
#' The function handles various metrics, including model comparison metrics (e.g., DIC, WAIC, etc.)
#' and random effect-related metrics.
#'
#' @param metric A character string representing the metric for which the y-axis label is needed.
#'   Supported metrics include:
#'   - `"dic"`, `"waic"`, `"lms"`, `"mae"`, `"rmse"`, `"crps"`, `"rsq"`,
#'   - `"dic_vs_first"`, `"waic_vs_first"`, `"mae_vs_first"`, `"rmse_vs_first"`,
#'   - `"crps_vs_first"`, and random effect metrics like `"re_n_var"` and `"re_n_var_change"`,
#'   - where `n` represents the random effect index, e.g., `"re_1_var"`, `"re_2_var_change"`.
#'
#' @return A character string representing the custom y-axis label for the provided metric.
#' 
#' @noRd

.get_metric_title <- function(metric) {
  # Default metric labels
  metric_labels <- c(
    dic = "Deviance Information Criterion (DIC)",
    waic = "Watanabe-Akaike Information Criterion (WAIC)",
    lms = "Least Mean Squares (LMS)",
    mae = "Mean Absolute Error (MAE)",
    rmse = "Root Mean Squared Error (RMSE)",
    crps = "Continuous Ranked Probability Score (CRPS)",
    rsq = "R-squared (R^2)",
    dic_vs_first = "Difference in DIC vs. reference model",
    waic_vs_first = "Difference in WAIC vs. reference model",
    mae_vs_first = "Difference in MAE vs. reference model",
    rmse_vs_first = "Difference in RMSE vs. reference model",
    crps_vs_first = "Difference in CRPS vs. reference model"
  )
  
  # Handle the special cases for random effect variables
  if (grepl("^re_\\d+_var$", metric)) {
    label <- "Random Effect Variance"
  } else if (grepl("^re_\\d+_var_change$", metric)) {
    label <- "Proportion change of the random effect variance \nvs. reference model"
  } else if (metric %in% names(metric_labels)) {
    label <- metric_labels[[metric]]
  } else {
    label <- metric  # return the original metric name if no match is found
  }
  
  return(label)
}



#' .log10p1_trans
#' 
#' Transform to execute a log10(x+1) scale transformation
#' 
#' @noRd
.log10p1_trans <- scales::trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse = function(x) 10^x - 1
)

#' .exp_trans
#' 
#' Transform to execute a exp(x) scale transformation
#' 
#' @noRd

.exp_trans <- scales::trans_new(
  name = "exp",
  transform = function(x) exp(x),
  inverse = function(x) log(x),
  breaks = scales::trans_breaks("exp", "log"),   # this line enables better axis ticks!
  format = scales::label_number(accuracy = 0.01),
  domain = c(-Inf, Inf)
)

#' .log10_breaks_like
#' 
#' Breaks and label generator for .log10p1_trans
#' @param x the numeric vector for which labels are computed
#' @noRd

.log10_breaks_like <- function(x) {
  rng <- range(x, na.rm = TRUE)
  upper <- ceiling(log10(rng[2] + 1))
  lower <- floor(log10(max(1, rng[1] + 1))) 
  breaks <- 10^(lower:upper)
  if(lower==0 & any(x<1, na.rm = TRUE)){
    breaks <- c(0, breaks )
  }
  return(breaks)
}



# plot_coef_nl_grid -----------------------------------------------------------
#' Plot Nonlinear Effects in a Grid Layout Across Models
#'
#' Generates a grid of plots displaying nonlinear effects  
#' across one or more fitted models within a \code{GHRmodels} object. 
#' For each effect:
#' \enumerate{
#'   \item A partial-effect plot is generated per model, colored by replicate where applicable.
#'   \item Optionally, a histogram showing the distribution of the corresponding 
#'   covariate is included beneath the partial-effect plot.
#'   \item Plots are arranged in a grid: rows = models, columns = effects.
#' }
#'
#' @param model A \code{GHRmodels} object containing fitted model outputs.
#' @param mod_id Character vector of model identifiers (must match entries in `model$mod_gof$model_id`).
#' @param mod_label Optional character vector of labels for each model (parallel to \code{mod_id}).
#'   Used to label the rows in the final grid.
#' @param name Optional character vector of variable names (as used in \code{inla.group(...)})
#'   to select specific nonlinear effects. If \code{NULL}, all nonlinear effects in the model are plotted.
#' @param pattern Optional regular expression pattern to match effect names.
#'   Used to select nonlinear effects when \code{name} is not provided.
#'   It will select any nonlinear effects containing the patterns specified.
#' @param var_label Optional named character vector providing custom labels for each nonlinear variable.
#' Names must match the variable names (e.g., from \code{inla.group(x)}), not the full effect name.
#' @param xlim Optional named list specifying x-axis limits for each effect.
#'   Each element should be a numeric vector of length 2: \code{list(var1 = c(min, max), var2 = c(min, max))}.
#'   Variable names must match those used in \code{inla.group()}.
#' @param ylab Optional y-axis label (default = "Effect size").
#' @param xlab Optional x-axis label (default = constructed from variable name).
#' @param title Optional overall title for the full plot grid.
#' @param palette Name of the color palette to use (passed to \code{GHR_palette}). Default is \code{"Qualitative"}.
#' @param histogram Logical; if \code{TRUE} (default), includes a histogram below each partial-effect plot.
#' @param legend Legend title for the replicate color scale (if multi-replicate effects are present). Default is \code{"Replicate"}.
#' @param hist_fill Fill color for histogram bars. Default is \code{"grey"}.
#' @param rug Include a rug plot for the x-axis.
#' @param exp Logical, if TRUE coefficients are exponentiated. Default is FALSE
#' 
#' @return A \code{ggplot} or \code{cowplot} object: a grid of nonlinear effect plots, with optional histograms
#'   and legends. Rows correspond to models; columns correspond to nonlinear effects.
#'
#' @noRd
#' @importFrom stats setNames median

# Modified version of `.plot_coef_nl_grid()` with shared x-axis label only at the bottom

.plot_coef_nl_grid <- function(model,
                               mod_id,
                               mod_label = NULL,
                               name = NULL,
                               pattern = NULL,
                               title = NULL,
                               var_label = NULL,
                               palette = "IDE2",
                               xlim = NULL,
                               ylab = NULL,
                               xlab = NULL,
                               histogram = FALSE,
                               legend = "Replicate",
                               hist_fill = "grey", 
                               rug = TRUE,
                               exp = FALSE){       
  
  # Helper to extract the shared legend from a ggplot object
  get_shared_legend <- function(p) {
    tmp <- suppressWarnings(ggplot2::ggplotGrob(p))
    legend_index <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if (length(legend_index) == 0) return(NULL)
    tmp$grobs[[legend_index]]
  }
  
  # Create default model label map; override with provided labels if given
  mod_label_map <- setNames(mod_id, mod_id)
  if (!is.null(mod_label)) {
    if (is.null(names(mod_label))) stop("'mod_label' must be a named vector")
    if (!all(names(mod_label) %in% mod_id)) stop("All names in 'mod_label' must be included in 'mod_id'")
    mod_label_map[names(mod_label)] <- mod_label
  }
  
  # Find all nonlinear (inla.group) effects across the selected models
  all_effects_by_model <- lapply(mod_id, function(m) {
    grep("inla\\.group", names(model$random[[m]]), value = TRUE)
  })
  all_unique_effects <- unique(unlist(all_effects_by_model))
  
  selected_nl_names <- character(0)  # Placeholder for selected nonlinear effect names
  
  # If 'name' is given, match nonlinear effect variables exactly
  if (!is.null(name)) {
    for (v in name) {
      pattern_v <- paste0("^INLA::inla\\.group\\(", v, "(\\)|,)")
      matched <- grep(pattern_v, all_unique_effects, value = TRUE)
      if (length(matched) == 0) warning(sprintf("Variable '%s' not found in any model", v))
      selected_nl_names <- c(selected_nl_names, matched)
    }
  }
  
  # If 'pattern' is given, match any effects containing the pattern
  if (!is.null(pattern)) {
    if (length(pattern) != 1) stop("The 'pattern' argument must be a single string (length 1).")
    matched_pattern <- grep(pattern, all_unique_effects, value = TRUE)
    if (length(matched_pattern) == 0) warning(sprintf("No nonlinear effect matched the pattern '%s'", pattern))
    selected_nl_names <- c(selected_nl_names, matched_pattern)
  }
  
  # Require 'name' or 'pattern' if multiple models are selected
  if (length(mod_id) > 1 && is.null(name) && is.null(pattern)) {
    stop("When plotting multiple models, you must specify 'name' or 'pattern'.")
  } else if (length(mod_id) == 1 && is.null(name) && is.null(pattern)) {
    selected_nl_names <- all_effects_by_model[[1]]  # Use all effects for single model
  }
  
  selected_nl_names <- unique(selected_nl_names)
  
  # Extract variable names from selected nonlinear effect names
  var_names_per_effect <- sapply(selected_nl_names, function(nl) {
    sub(".*inla\\.group\\(([^,]+).*", "\\1", nl)
  })
  
  # Helper function to get per-variable x-axis limits from `xlim` list
  parse_xlim <- function(xlim, var_names) {
    xmins <- rep(NA, length(var_names))
    xmaxs <- rep(NA, length(var_names))
    if (is.null(xlim)) return(list(xmin = xmins, xmax = xmaxs))
    for (i in seq_along(var_names)) {
      v <- var_names[i]
      if (v %in% names(xlim)) {
        lim <- xlim[[v]]
        xmins[i] <- lim[1]
        xmaxs[i] <- lim[2]
      }
    }
    list(xmin = xmins, xmax = xmaxs)
  }
  
  xlim_parsed <- parse_xlim(xlim, var_names_per_effect)
  xmin_vec <- xlim_parsed$xmin
  xmax_vec <- xlim_parsed$xmax
  
  grid_list <- list()              # To store each plot panel
  effect_labels <- character(length(selected_nl_names))  # Human-readable effect labels
  replicate_legend <- NULL        # Placeholder for shared legend
  
  # Loop over each selected nonlinear effect
  for (i in seq_along(selected_nl_names)) {
    nl_name <- selected_nl_names[i]
    var_extracted <- var_names_per_effect[i]
    effect_label <- if (!is.null(var_label) && var_extracted %in% names(var_label)) var_label[[var_extracted]] else var_extracted
    effect_labels[i] <- effect_label
    
    # Loop through each model
    for (m in mod_id) {
      if (!(nl_name %in% names(model$random[[m]]))) next
      key <- paste0(m, "_", effect_label)
      
      df0 <- model$random[[m]][[nl_name]]
      colnames(df0)[1:8] <- c("ID", "mean", "sd", "lci", "est", "uci", "mode", "kld")
      df0$ID <- as.numeric(as.character(df0$ID))
      
      # Restrict x-axis range if limits were specified
      x_min_use <- if (!is.na(xmin_vec[i])) xmin_vec[i] else min(df0$ID, na.rm = TRUE)
      x_max_use <- if (!is.na(xmax_vec[i])) xmax_vec[i] else max(df0$ID, na.rm = TRUE)
      df0 <- df0[df0$ID >= x_min_use & df0$ID <= x_max_use, , drop = FALSE]
      
      # Determine how many replicates are in the data
      n_unique <- length(unique(df0$ID))
      n_rep <- nrow(df0) / n_unique
      df0$replicate <- factor(rep(seq_len(n_rep), each = n_unique))
      df0$replicate_count <- n_rep
      
      df0_single <- df0[df0$replicate_count == 1, ]
      df0_multi  <- df0[df0$replicate_count > 1, ]
      
      # Set axis labels
      xlab_use <- effect_label
      ylab_use <- if (is.null(ylab)) "Effect size" else ylab
      
      # Start ggplot for lineplot
      p_partial <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$ID, y = .data$mean)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey80", linewidth = 0.5)
      
      # Single-replicate plot with ribbon and line
      if (nrow(df0_single) > 0) {
        my_palette <- rev(GHRexplore::GHR_palette(palette, 2)(2))[1]
        p_partial <- p_partial +
          ggplot2::geom_ribbon(data = df0_single,
                               mapping = ggplot2::aes(ymin = .data$lci, ymax = .data$uci),
                               fill = my_palette, alpha = 0.2) +
          ggplot2::geom_line(data = df0_single,
                             mapping = ggplot2::aes(y = .data$est),
                             color = my_palette, linewidth = 0.5)
      }
      
      # Multi-replicate: multiple colored lines
      if (nrow(df0_multi) > 0) {
        n_colors <- length(unique(df0_multi$replicate))
        my_palette_multi <- rev(GHRexplore::GHR_palette(palette, n_colors)(n_colors))
        
        p_partial <- p_partial +
          ggplot2::geom_line(data = df0_multi,
                             mapping = ggplot2::aes(y = .data$est, color = .data$replicate, group = .data$replicate),
                             linewidth = 0.7) +
          ggplot2::scale_color_manual(values = my_palette_multi, name = legend) 
        
        # Extract legend for the first replicate plot
        if (is.null(replicate_legend)) {
          p_with_scale <- p_partial +
            ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal", legend.key = ggplot2::element_blank())
          replicate_legend <- get_shared_legend(p_with_scale)
        }
      }
      
      # Add rug (tick marks) to show data distribution
      if (isTRUE(rug)) {
        x_vals <- model$data[[var_extracted]]
        x_vals <- x_vals[x_vals >= x_min_use & x_vals <= x_max_use]
        df_rug <- data.frame(x_value = x_vals)
        p_partial <- p_partial + 
          ggplot2::geom_rug(data = df_rug,
                            ggplot2::aes(x = .data$x_value, y = NULL, 
                                         color = I("grey20"), fill = NULL),
                            lwd = 0.03, alpha = 0.7, sides = "b")
      }
      
      # Optionally transform the y-axis with exp()
      if (exp) {
        p_partial <- p_partial + ggplot2::scale_y_continuous(transform = .exp_trans,
                                                             labels = function(y) exp(y)) 
      }
      
      # Final plot theming
      p_partial <- p_partial +
        ggplot2::labs(x = xlab_use, y = ylab_use) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(legend.position = "none")
      
      # Add optional histogram below the main plot
      p_hist <- NULL
      if (histogram && var_extracted %in% names(model$data)) {
        x_vals <- model$data[[var_extracted]]
        x_vals <- x_vals[x_vals >= x_min_use & x_vals <= x_max_use]
        if (length(x_vals) > 0) {
          df_hist_local <- data.frame(x_value = x_vals)
          p_hist <- ggplot2::ggplot(df_hist_local, ggplot2::aes(x = .data$x_value)) +
            ggplot2::geom_histogram(fill = hist_fill, color = "white", bins = 50) +
            ggplot2::labs(x = NULL, y = "Count") +
            ggplot2::theme_bw(base_size = 12) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                           axis.ticks.x = ggplot2::element_blank(),
                           axis.title.x = ggplot2::element_blank())
        }
      }
      
      # Combine effect plot with histogram, if applicable
      combined <- if (!is.null(p_hist)) {
        cowplot::plot_grid(p_partial, p_hist, ncol = 1, align = "v", rel_heights = c(3, 1))
      } else {
        p_partial
      }
      
      # Store in grid list for later composition
      grid_list[[key]] <- combined
    }
  }
  
  # Create plot rows: one row per model, with all effects
  row_list <- lapply(mod_id, function(m) {
    row_plots <- lapply(effect_labels, function(eff_lab) {
      key <- paste0(m, "_", eff_lab)
      grid_list[[key]]
    })
    row_plots <- Filter(Negate(is.null), row_plots)
    if (length(row_plots) == 0) return(NULL)
    row_combined <- cowplot::plot_grid(plotlist = row_plots, nrow = 1, align = "hv")
    cowplot::plot_grid(cowplot::ggdraw() + cowplot::draw_label(mod_label_map[[m]], angle = 90),
                       row_combined, ncol = 2, rel_widths = c(0.05, 1))
  })
  
  # Filter out empty rows
  row_list <- Filter(Negate(is.null), row_list)
  if (length(row_list) == 0) {
    return(NULL)
  }
  
  # Assemble full plot grid
  full_grid <- cowplot::plot_grid(plotlist = row_list, ncol = 1)
  
  # Add shared legend, if needed
  if (!is.null(replicate_legend)) {
    full_grid <- cowplot::plot_grid(full_grid,
                                    cowplot::plot_grid(NULL, replicate_legend, ncol = 1),
                                    ncol = 1, rel_heights = c(1, 0.1))
  }
  
  # Add title, if provided
  if (!is.null(title)) {
    full_grid <- cowplot::plot_grid(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title),
                                    full_grid, ncol = 1, rel_heights = c(0.08, 1))
  }
  
  return(full_grid)
}



# plot_coef_nl_collapse -----------------------------------------------------------


#' Plot Nonlinear Effects from Multiple INLA Models (in a single plot per effect)
#'
#' This function plots the estimated nonlinear effects of specified variables
#' from multiple INLA models. It supports side-by-side comparisons, shared legends,
#' and optional histograms of covariate distributions.
#'
#' @param model A list-like object containing INLA model outputs with components `random` and `data`.
#' @param mod_id Character vector of model identifiers (must match entries in `model$mod_gof$model_id`).
#' @param mod_label Optional character vector of labels for each model (must match `mod_id` in length).
#' @param name Character vector of variable names to be plotted.
#' @param title Optional overall plot title.
#' @param var_label Optional named vector of labels for variables (names = variable names, values = labels).
#' @param palette Character string indicating the color palette name passed to `GHR_palette()`.
#' @param xlim Optional named list of x-axis limits per variable (e.g., `list("pdsi" = c(-5, 5))`).
#' @param ylab Optional y-axis label (default = "Effect size").
#' @param xlab Optional x-axis label (default = "Values").
#' @param histogram Logical; if `TRUE`, includes a histogram of the variable's distribution.
#' @param legend Character string indicating the legend title (default = "Model").
#' @param hist_fill Fill color for histogram bars.
#' @param rug Include a rug plot for the x-axis.
#' @param exp Logical, if TRUE coefficients are exponentiated. Default is FALSE

#' @return A `cowplot` object combining nonlinear effect plots and a shared legend.
#' @examples
#' \donttest{
#' plot_coef_nl_collapse(
#'   model = model_test,
#'   mod_id = c("mod6", "mod10"), 
#'   mod_label = c("tmin.l2 + pdsi", "tmin.l2 + pdsi.nl"),
#'   name = c("tmin.l2", "pdsi"),
#'   title = "Exposure - Response",
#'   var_label = c("tmin.l2" = "T min lag 2", 
#'                "pdsi" = "Prec. lag 0"),
#'   xlim = list("tmin.l2" = c(15,22)),
#'   palette = "IDExtremes", 
#'   histogram = TRUE
#')
#'}
#' @noRd
#' 
#' @importFrom rlang .data
#' 
#' @importFrom stats setNames median

.plot_coef_nl_collapse <- function(model,
                                   mod_id,
                                   mod_label = NULL,
                                   name = NULL,
                                   title = NULL,
                                   var_label = NULL,
                                   palette = "IDE2",
                                   xlim = NULL,
                                   ylab = NULL,
                                   xlab = NULL,
                                   histogram = FALSE,
                                   legend = "Model",
                                   hist_fill = "grey",
                                   rug = TRUE,
                                   exp = FALSE) {
  get_shared_legend <- function(p) {
    tmp <- suppressWarnings(ggplot2::ggplotGrob(p))
    legend_index <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if (length(legend_index) == 0) return(NULL)
    tmp$grobs[[legend_index]]
  }
  
  if (length(name) != 1) stop("'name' must be a single variable name.")
  v <- name
  
  missing_mods <- setdiff(mod_id, model$mod_gof[["model_id"]])
  if (length(missing_mods) > 0) {
    stop(paste("Missing model(s):", paste(missing_mods, collapse = ", ")))
  }
  
  mod_label_map <- setNames(mod_id, mod_id)
  if (!is.null(mod_label)) {
    if (is.null(names(mod_label))) stop("'mod_label' must be a named vector")
    if (!all(names(mod_label) %in% mod_id)) stop("All names in 'mod_label' must be included in 'mod_id'")
    mod_label_map[names(mod_label)] <- mod_label
  }
  
  all_effects <- unique(unlist(lapply(mod_id, function(m) {
    grep("inla\\.group", names(model$random[[m]]), value = TRUE)
  })))
  
  matched_effect <- grep(paste0("^INLA::inla\\.group\\(", v, "(\\)|,)"), all_effects, value = TRUE)
  if (length(matched_effect) == 0) warning(sprintf("Variable '%s' not found in any model", v))
  effect_name <- matched_effect[1]
  
  df_plot <- do.call(rbind, lapply(mod_id, function(m) {
    if (!(effect_name %in% names(model$random[[m]]))) return(NULL)
    df <- model$random[[m]][[effect_name]]
    if (ncol(df) < 8) return(NULL)
    colnames(df)[1:8] <- c("ID", "mean", "sd", "lci", "est", "uci", "mode", "kld")
    df$ID <- as.numeric(as.character(df$ID))
    if (!is.null(xlim) && v %in% names(xlim)) {
      lim <- xlim[[v]]
      df <- df[df$ID >= lim[1] & df$ID <= lim[2], ]
    }
    df$model <- m
    df$model_label <- mod_label_map[[m]]
    return(df)
  }))
  
  if (is.null(df_plot) || nrow(df_plot) == 0) {
    return(NULL)
  }
  
  used_model_labels <- unique(df_plot$model_label)
  palette_named <- setNames(
    rev(GHRexplore::GHR_palette(palette, length(used_model_labels))(length(used_model_labels))),
    used_model_labels
  )
  
  v_label <- if (!is.null(var_label) && v %in% names(var_label)) var_label[[v]] else v
  this_xlab <- if (!is.null(xlab)) xlab else v_label
  this_ylab <- if (!is.null(ylab)) ylab else "Effect size"
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$ID, y = .data$est,
                                             color = .data$model_label, fill = .data$model_label)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey80", linetype = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lci, ymax = .data$uci), alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_color_manual(values = palette_named, name = legend) +
    ggplot2::scale_fill_manual(values = palette_named, name = legend)
  
  if(isTRUE(rug)){
    x_vals <- model$data[[v]]
    x_vals <- x_vals[x_vals >= min(df_plot$ID, na.rm = TRUE)]
    x_vals <- x_vals[x_vals <= max(df_plot$ID, na.rm = TRUE)]
    
    if (!is.null(xlim) && v %in% names(xlim)) {
      lim <- xlim[[v]]
      x_vals <- x_vals[x_vals >= lim[1] & x_vals <= lim[2]]
    }    
    df_rug <- data.frame(x_value = x_vals)
    p <- p + 
      ggplot2::geom_rug(data = df_rug, 
                        ggplot2::aes(x = .data$x_value, y = NULL, 
                                     color = I("grey20"), fill = NULL),
                        lwd = 0.03, alpha = 0.7, sides = "b")
  }
  
  if (exp) {
    p <- p + ggplot2::scale_y_continuous(transform = .exp_trans,
                                         labels = function(y) exp(y))
  }
  
  p <- p +
    ggplot2::labs(x = this_xlab, y = this_ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  if (histogram && v %in% names(model$data)) {
    x_vals <- model$data[[v]]
    if (!is.null(xlim) && v %in% names(xlim)) {
      lim <- xlim[[v]]
      x_vals <- x_vals[x_vals >= lim[1] & x_vals <= lim[2]]
    }
    if (length(x_vals) > 0) {
      df_hist <- data.frame(x_value = x_vals)
      p_hist <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$x_value)) +
        ggplot2::geom_histogram(fill = hist_fill, color = "white", bins = 50) +
        ggplot2::labs(x = NULL, y = "Count") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank())
      p <- cowplot::plot_grid(p, p_hist, ncol = 1, align = "v", rel_heights = c(3, 1))
    }
  }
  
  attr(p, "used_model_labels") <- used_model_labels
  attr(p, "palette_named") <- palette_named
  
  dummy_data <- do.call(rbind, lapply(used_model_labels, function(mod) {
    data.frame(x = c(1, 2), y = c(0.1, 0.2), lci = c(0.05, 0.15), uci = c(0.15, 0.25), model_label = mod)
  }))
  
  legend_title <- if (is.null(legend)) "Model" else legend
  
  p_dummy <- ggplot2::ggplot(dummy_data, ggplot2::aes(x = .data$x, y = .data$y,
                                                      color = .data$model_label, fill = .data$model_label)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lci, ymax = .data$uci), alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_color_manual(values = palette_named, name = legend_title) +
    ggplot2::scale_fill_manual(values = palette_named, name = legend_title) +
    ggplot2::theme(legend.position = "right",
                   legend.background = ggplot2::element_rect(fill = NA),
                   legend.key = ggplot2::element_rect(fill = NA))
  
  legend_plot <- get_shared_legend(p_dummy)
  
  final_plot <- cowplot::plot_grid(
    p,
    legend_plot,
    ncol = 2,
    rel_widths = c(0.7, 0.3)
  )
  
  if (!is.null(title)) {
    final_plot <- cowplot::plot_grid(
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title),
      final_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
  }
  
  return(final_plot)
}
