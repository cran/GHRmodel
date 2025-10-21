#' @title Plot Models by Goodness-of-Fit
#'
#' @description
#' Provides visualization of model performance using selected goodness-of-fit (GoF) metrics for one or more models.
#' It is typically used with the `mod_gof` component of a `GHRmodels` object 
#' (produced by \code{\link{fit_models}}), but it can also accept any custom 
#' data frame — provided it contains the same column names as the default
#' `mod_gof` output (including `model_id` and the relevant metric column names).
#'  It supports visual grouping by aesthetics (color, shape, facet), arranging models by metric, 
#'  and adding credible intervals for model differences.
#'
#' @param mod_gof A data frame containing goodness-of-fit statistics for each model.
#' Typically this is the \code{mod_gof} component of a \code{GHRmodels} object. It must include at least
#' a \code{model_id} column and the selected \code{metric}. Other columns can be used for aesthetics (e.g., color, shape).
#' @param metric Character string specifying the GoF metric to plot. Common options include:
#' \itemize{
#'   \item \code{"dic"}, \code{"waic"}, \code{"lms"}, \code{"mae"}, \code{"rmse"}, \code{"crps"}, \code{"rsq"}
#'   \item Differences from baseline: \code{"dic_vs_first"}, \code{"waic_vs_first"}, \code{"mae_vs_first"}, etc.
#'   \item Random effect variances: \code{"re_n_var"}, \code{"re_n_var_change"}, where \code{n} is an index.
#' }
#'
#' @param mod_id Optional character vector of model IDs to include. If \code{NULL}, includes all in \code{mod_gof}.
#' @param mod_label Optional named or unnamed vector to customize display names for models.
#' If unnamed, must match the order of \code{mod_id}.
#' @param ci Logical. If \code{TRUE}, adds credible intervals for \code{"*_vs_first"} metrics (if available).
#' @param var_arrange Character string for a column name used to order models along the x-axis.
#' Defaults to \code{"model_id"} order if \code{NULL}.
#' @param var_color Optional; name of a column in \code{mod_gof} to use for color grouping.
#' @param var_shape Optional; name of a column in \code{mod_gof} to use for point shape grouping.
#' @param var_facet Optional; name of a column in \code{mod_gof} to use for faceting the plot.
#' @param palette Character; name of a color palette to use if \code{var_color} is provided. 
#' Default is \code{"IDE2"}.
#' @return A \code{ggplot2} object showing the specified metric for each model, optionally grouped and faceted.
#' The plot supports:
#' \itemize{
#'   \item Ranking or sorting models by a specified variable
#'   \item Highlighting credible intervals for relative metrics (e.g. \code{"dic_vs_first"})
#'   \item Group-level comparisons via color, shape, and facet aesthetics
#' }
#'
#' @details
#' This function helps interpret and visualize comparative model performance:
#' \itemize{
#'   \item Relative metrics (e.g., \code{"*_vs_first"}) assume the first model is a reference.
#'   \item If \code{ci = TRUE}, the function looks for columns like \code{"dic_vs_first_lci"} and \code{"_uci"}.
#'   \item The user can customize model order with \code{var_arrange} and legend groupings using \code{var_color}, etc.
#' }
#' 
#' @importFrom stats setNames median
#' 
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Plot models by difference in DIC
#'         
#' plot_gof(mod_gof = model_list$mod_gof,
#'         metric = "dic_vs_first",
#'         ci = TRUE,
#'         var_arrange = "dic",
#'         var_color = "covariate_1",
#'         var_shape = "covariate_2",
#'         palette= "IDE2")
#' }
#' 
#' @seealso 
#' \code{\link{fit_models}} for fitting multiple INLA models.
#' @name plot_gof
#' @rdname plot_gof
#' @export
#'
#'
plot_gof <- function(mod_gof,
                     metric = "dic",
                     mod_id = NULL,
                     mod_label = NULL,
                     ci = FALSE,
                     var_arrange = NULL,
                     var_color = NULL,
                     var_shape = NULL,
                     var_facet = NULL,
                     palette = "IDE2") {
  
  # Input validation
  if (missing(mod_gof)) stop("Missing 'mod_gof'")
  if (!is.data.frame(mod_gof)) stop("'mod_gof' should be a data.frame")
  
  # Filter models by mod_id if provided
  if (!is.null(mod_id)) {
    mod_gof <- mod_gof |> dplyr::filter(.data[["model_id"]] %in% mod_id)
  } else {
    mod_id <- mod_gof$model_id
  }
  

  # Apply mod_label logic
  if (!is.null(mod_label)) {
    if (!is.null(names(mod_label))) {
      unmatched <- setdiff(mod_id, names(mod_label))
      if (length(unmatched) > 0) {
        stop("mod_label must be a named vector with names matching mod_id or model_id.", call. = FALSE)
      }
      mod_label_full <- setNames(mod_id, mod_id)
      mod_label_full[names(mod_label)] <- mod_label
      mod_label <- mod_label_full
    } else {
      if (length(mod_label) != length(mod_id)) {
        stop("mod_label must be a named vector or have the same length as mod_id.", call. = FALSE)
      }
      names(mod_label) <- mod_id
    }
  } else {
    mod_label <- stats::setNames(mod_id, mod_id)
  }
  
  # Assign factor labels to ensure plotting order
  mod_gof <- mod_gof[order(match(mod_gof$model_id, mod_id)), ]
  mod_gof$label <- factor(mod_label[mod_gof$model_id], levels = mod_label)
  
  # Optional sort by another variable
  if (!is.null(var_arrange)) {
    mod_gof <- mod_gof[order(mod_gof[[var_arrange]]), ]
    mod_gof$label <- factor(mod_gof$label, levels = mod_gof$label)
  }
  
  # Add aesthetic grouping variables
  if (!is.null(var_color)) {
    mod_gof$variable_color <- .plotting_variable(mod_gof[[var_color]])
  }
  if (!is.null(var_shape)) {
    mod_gof$variable_shape <- .plotting_variable(mod_gof[[var_shape]])
  }
  if (!is.null(var_facet)) {
    mod_gof$variable_facet <- .plotting_variable(mod_gof[[var_facet]])
  }
  
  # Title for color legend
  color_legend_title <- if (!is.null(var_color) && var_color %in%
                            c("dic", "waic", "lms", "mae", "rmse", "crps", "rsq",
                              "dic_vs_first", "waic_vs_first", "mae_vs_first",
                              "rmse_vs_first", "crps_vs_first")) {
    .get_metric_title(var_color)
  } else if (!is.null(var_color) && grepl("^re_\\d+_var$", var_color)) {
    paste("Random Effect", sub("^re_(\\d+)_var$", "\\1", var_color), "Variance")
  } else if (!is.null(var_color) && grepl("^re_\\d+_var_change$", var_color)) {
    paste("Proportion change of Random Effect",
          sub("^re_(\\d+)_var_change$", "\\1", var_color),
          "Variance vs. reference model")
  } else {
    var_color
  }
  
  # Build ggplot
  p <- ggplot2::ggplot(mod_gof, ggplot2::aes(x = label, y = .data[[metric]]))
  
  # Add point layers
  if (!is.null(var_color) && !is.null(var_shape)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = variable_color, shape = variable_shape), size = 2)
  } else if (!is.null(var_color)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = variable_color), size = 2)
  } else if (!is.null(var_shape)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(shape = variable_shape), color = "red", size = 2)
  } else {
    p <- p + ggplot2::geom_point(color = "red", size = 2)
  }
  
  # Manual color scale if needed
  if (!is.null(var_color)) {
    ncolor <- length(unique(mod_gof$variable_color))
    my_palette <- rev(GHRexplore::GHR_palette(palette)(ncolor))
    p <- p + ggplot2::scale_color_manual(values = my_palette, name = color_legend_title)
  }
  
  # Optional credible interval bars
  if (ci && metric %in% c("dic_vs_first", "waic_vs_first")) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
    lci <- paste0(metric, "_lci")
    uci <- paste0(metric, "_uci")
    if (!is.null(var_color)) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(
        ymin = .data[[lci]],
        ymax = .data[[uci]],
        color = variable_color
      ), width = 0)
    } else {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(
        ymin = .data[[lci]],
        ymax = .data[[uci]]
      ), color = "red", width = 0)
    }
  }
  
  # Final plot labels and theme
  p <- p +
    ggplot2::labs(
      x = "Model ID",
      y = .get_metric_label(metric, ci = ci),
      title = .get_metric_title(metric),
      shape = var_shape
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  # Facet if needed
  if (!is.null(var_facet)) {
    p <- p + ggplot2::facet_grid(~variable_facet, scales = "free")
  }
  
  return(p)
}
