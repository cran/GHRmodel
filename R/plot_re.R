#' Plot Random Effects from GHRmodels
#'
#' @title Plot Random Effects
#'
#' @description
#' 
#' Generates plots of random effects from one or more fitted models contained within a `GHRmodels` object.
#' The function supports two main display modes: 
#' - Caterpillar plot of effect sizes with uncertainty intervals (the default). 
#' - Choropleth map (when a spatial map (\code{sf} object) is provided in the `map` argument).
#'
#' It also supports visualization of replicated or grouped effects via the `rep_id` argument.
#' 
#'
#' @param models A \code{GHRmodels} object containing fitted models and random effects.
#' @param mod_id Character vector of model IDs to plot (must match entries in \code{models$mod_gof$model_id}).
#' @param re_id Character; name of the variable defining the random effect (from \code{models$re}).
#' @param rep_id Optional character string; name of a grouping variable if random effects are replicated.
#' Default is \code{NULL}.
#' @param map Optional \code{sf} object providing spatial geometry. If \code{NULL}, returns a caterpillar plot.
#' @param map_area Character; column name in \code{map} indicating spatial units (must match \code{re_id} order).
#' @param mod_label Optional labels for models. Can be a named vector (e.g., \code{c("mod1" = "Baseline", "mod2" = "Adjusted")}) 
#' or an unnamed vector with the same order as \code{mod_id}.
#' @param re_label Optional; variable in the data to label the random effect units (e.g., year names instead of numeric IDs).
#' @param rep_label Optional; label for replicated grouping variable (e.g., for years or time periods).
#' @param ref_color Color used for the reference model. If specified, this will apply to the first model in \code{mod_id}.
#' @param palette Character; name of the color palette to use. Defaults to \code{"IDE1"} for maps and \code{"IDE2"} otherwise.
#' @param var_arrange Character; how to arrange REs on the x-axis. Options: \code{"median"} or \code{"ID"}. Default is \code{"ID"}.
#' @param title Title for the plot.
#' @param centering Value at which to center the color scale for map plots. Default is \code{0}.
#' @param ylab Label for the y-axis. Default is \code{"Effect Size"}.
#' @param xlab Label for the x-axis. Default is \code{"Re ID"}.
#' @param legend Label for the legend in map plots. Default is \code{"Effect Size"}.
#' @param exp Logical; if \code{TRUE}, exponentiates the effects (useful for log-scale models). Default is \code{FALSE}.
#'
#' @return
#' A \code{ggplot2} plot object:
#' \itemize{
#'   \item If \code{map} is \code{NULL}, returns a caterpillar plot showing median REs with 95% uncertainty intervals.
#'   \item If \code{map} is provided, returns a faceted choropleth map showing RE medians by area and (optionally) replicate.
#' }
#'
#' @details
#' \itemize{
#'   \item If \code{map} is used, \code{map_area} must match a column in \code{map} and correspond in order to the RE unit.
#'   \item For BYM/BYM2 models, only the total random effect is plotted (structured/unstructured parts are merged).
#'   \item When no map is used, the plot compares models via colored points and intervals for each RE unit.
#'   \item Replicated REs (e.g., for years) can be plotted across facets using \code{rep_label}.
#'   \item Model comparison is visually aided using distinct colors; the first model in \code{mod_id} is the reference.
#' }
#'
#' @seealso 
#' \code{\link{fit_models}} for model fitting; \code{\link{as_GHRformulas}} for formula setup.
#' 
#' @importFrom stats setNames median
#' 
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' #  Plot the estimated yearly random effects for three different models.
#' plot_re(
#'   model = model_list,                          # A GHRmodels object 
#'   mod_id = c("mod1", "mod3", "mod5"),          # IDs of the models 
#'   mod_label = c("Baseline",                    # Custom labels for the models
#'                 "tmin.l1_nl",           
#'                 "pdsi.l1_nl + tmin.l1_nl"),     
#'   re_id = "year_id",                           # Name of the random effect variable 
#'   re_label = "year",                           # Label to map year_id to calendar years
#'   ref_color = "grey",                          # Color for the reference model’s effects
#'   palette = "IDE2",                            # Color for other model effects
#'   title = "Yearly Random Effect",              # Title for the plot
#'   xlab = "Year"                                # Label for the x-axis 
#' )
#' }
#'
#' @rdname plot_re
#' @export

# ---- plot_re function with comments and numeric mod_id ordering ----
plot_re <- function(models,
                    mod_id,
                    re_id,
                    rep_id = NULL,
                    map = NULL,
                    map_area = NULL,
                    mod_label = NULL,
                    re_label = NULL,
                    rep_label = NULL, 
                    ref_color = NULL,
                    palette = NULL, 
                    var_arrange = "ID",
                    title = "", 
                    xlab = "Re ID",
                    ylab = "Effect Size", 
                    legend = "Effect Size",
                    centering = 0,
                    exp = FALSE) {
  
  # Validate models object and required parameters
  if (!inherits(models, "GHRmodels")) stop("'models' must be a GHRmodels object.")
  if (missing(re_id) || missing(mod_id)) stop("Both 're_id' and 'mod_id' must be provided.")
  
  # Validate model IDs
  invalid_models <- setdiff(mod_id, models$mod_gof$model_id)
  if (length(invalid_models) > 0) stop("Unknown mod_id(s): ", paste(invalid_models, collapse = ", "))
  
  # Validate re_id
  if (!re_id %in% names(models$data)) {
    stop("'re_id' must be a column name in models$data. Provided value '", re_id, "' was not found.")
  }
  
  # Validate map parameters if map is provided
  if (!is.null(map)) {
    if (is.null(map_area)) stop("If 'map' is provided, 'map_area' must be specified.")
    if (!map_area %in% names(map)) stop("'map_area' is not a column in the provided map.")
  }
  
  # Assign mod_label correctly
  if (!is.null(mod_label)) {
    if (!is.null(names(mod_label))) {
      full_label <- setNames(mod_id, mod_id)
      full_label[names(mod_label)] <- mod_label
      mod_label <- full_label
    } else {
      if (length(mod_label) != length(mod_id)) {
        stop("`mod_label` must be a named vector or have the same length as `mod_id`.", call. = FALSE)
      }
      names(mod_label) <- mod_id
    }
  } else {
    mod_label <- stats::setNames(mod_id, mod_id)
  }
  
  # Validate optional label columns
  if (!is.null(re_label) && !(re_label %in% names(models$data))) {
    stop(paste0("'re_label' (", re_label, ") is not a column in 'models$data'."))
  }
  if (!is.null(rep_label) && !(rep_label %in% names(models$data))) {
    stop(paste0("'rep_label' (", rep_label, ") is not a column in 'models$data'."))
  }
  
  if (is.null(palette)) {
    palette <- if (is.null(map)) "IDE2" else "IDE1"
  }
  
  # Detect if random effect is BYM-type
  random_expr <- models$re[which(grepl(re_id, models$re))]
  is_bym <- any(grepl("bym|bym2", random_expr))
  
  # Extract RE values and determine replication
  re_df_main <- models$random[[mod_id[1]]][[re_id]]
  n_re_id <- length(unique(re_df_main$ID))
  replicated <- any(duplicated(re_df_main$ID))
  
  # Check rep_id presence if needed
  if (replicated && is.null(rep_id)) stop("Detected replicated random effect, please provide 'rep_id'.")
  if (!replicated && !is.null(rep_id)) stop("'rep_id' provided but random effect is not replicated.")
  
  # Check if rep_id is correct 
  if (replicated) {
    # Number of unique rep_id values from the data
    n_rep_id <- length(unique(models$data[[rep_id]]))
    
    # Number of repetitions inferred from the random effects structure
    n_rep_random <- length(re_df_main$ID) / length(unique(re_df_main$ID))
    
    # Check if these lengths are equal
    if (n_rep_id != n_rep_random) {
      stop("Check rep_id: Number of unique 'rep_id' values in data (", n_rep_id, 
           ") does not match number of repetitions in random effects (", n_rep_random, ").")
    }
  }
  
  
  
  # Extract correct index range if BYM
  ranges <- if (is_bym) {
    starts <- seq(1, nrow(re_df_main), by = n_re_id)
    ends <- pmin(starts + (n_re_id / 2) - 1, nrow(re_df_main))
    as.vector(mapply(seq, starts, ends))
  } else {
    seq_len(nrow(re_df_main))
  }
  
  # Optional re/rep label mapping
  if (!is.null(re_label)) {
    re_id_label <- dplyr::bind_cols(ID = models$data[[re_id]],
                                    re_label = models$data[[re_label]]) |>
      dplyr::distinct()
  }
  if (!is.null(rep_label)) {
    rep_id_label <- dplyr::bind_cols(rep_id = models$data[[rep_id]],
                                     rep_label = models$data[[rep_label]]) |>
      dplyr::distinct()
  }
  
  # Build RE data for each model
  df_list <- lapply(seq_along(mod_id), function(i) {
    mod_name <- mod_id[i]
    df <- models$random[[mod_name]][[re_id]][ranges, c("ID", "0.025quant", "0.5quant", "0.975quant")]
    names(df) <- c("ID", "lci", "median", "uci")
    df$type <- mod_label[mod_name]
    df$rep_id <- if (replicated) {
      rep(seq_len(length(unique(models$data[[rep_id]]))), each = length(unique(models$data[[re_id]])))
    } else 1
    df
  })
  df_plot <- do.call(rbind, df_list)
  
  # Add labels
  if (!is.null(re_label)) {
    df_plot <- dplyr::left_join(df_plot, re_id_label, by = "ID")
  } else {
    df_plot$re_label <- as.character(df_plot$ID)
  }
  if (!is.null(rep_id)) {
    if (!is.null(rep_label)) {
      df_plot <- dplyr::left_join(df_plot, rep_id_label, by = "rep_id")
    } else {
      df_plot <- df_plot |>
        dplyr::mutate(rep_label = as.character(rep_id)) |>
        dplyr::mutate(rep_label = factor(rep_label, levels = unique(rep_label[order(as.numeric(rep_id))])))
    }
  }
  
  # Model label order
  df_plot$type <- factor(df_plot$type, levels = mod_label)
  
  # RE sorting
  arrange_var <- if (var_arrange %in% names(df_plot)) var_arrange else "ID"
  df_plot <- df_plot |> 
    dplyr::group_by(.data[["type"]]) |> 
    dplyr::arrange(.data[[arrange_var]], .by_group = TRUE) |> 
    dplyr::mutate(re_label = factor(.data[["re_label"]], levels = unique(.data[["re_label"]]))) |> 
    dplyr::ungroup()
  
  # ---- Plot without map ----
  if (is.null(map)) {
    unique_types <- unique(df_plot$type)
    ntype <- length(unique_types)
    my_palette <- rev(GHRexplore::GHR_palette(palette, ntype)(ntype))
    color_values <- stats::setNames(my_palette[seq_len(ntype)], levels(df_plot$type))
    if (!is.null(ref_color)) {
      color_values[levels(df_plot$type)[1]] <- ref_color
    }
    
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[["re_label"]],
                                               colour = .data[["type"]])) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["lci"]], ymax = .data[["uci"]]),
                             width = 0.0, alpha = 0.8, position = ggplot2::position_dodge(0.5)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::geom_point(ggplot2::aes(y = .data[["median"]]),
                          alpha = 0.8, position = ggplot2::position_dodge(0.5)) +
      ggplot2::scale_colour_manual(name = "Model", values = color_values) +
      ggplot2::labs(x = xlab, y = ylab, colour = NULL, title = title) +
      ggplot2::theme_bw()
    
    if (exp) {
      p <- p + ggplot2::scale_y_continuous(transform = .exp_trans,
                                           labels = function(x) round(exp(x), 2))
    } 
    if (!is.null(re_label)) {
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5))
    }
    if (replicated) {
      p <- p + ggplot2::facet_wrap(~ rep_label,
                                   labeller = ggplot2::label_wrap_gen(width = 15))
    }
    
    # ---- Plot with map ----
  } else {
    df_plot$rep_id <- as.factor(df_plot$rep_id)
    merged_map <- map
    merged_map$ID <- as.numeric(factor(merged_map[[map_area]]))
    if (replicated) {
      merged_map <- dplyr::bind_rows(replicate(length(unique(df_plot$rep_id)), merged_map, simplify = FALSE))
      merged_map$rep_id <- rep(unique(df_plot$rep_id), each = nrow(map))
      merged_map <- merge(merged_map, df_plot, by = c("ID", "rep_id"))
    } else {
      merged_map <- merge(merged_map, df_plot, by = "ID")
    }
    
    if (exp) {
      merged_map$exp_median <- exp(merged_map$median)
      min_val <- min(merged_map$exp_median, na.rm = TRUE)
      max_val <- max(merged_map$exp_median, na.rm = TRUE)
      center_val <- 1
      rescaled_center <- (center_val - min_val) / (max_val - min_val)
      my_palette <- GHRexplore::GHR_palette(palette, ncols = 9)(9)
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = merged_map, ggplot2::aes(fill = exp_median)) +
        ggplot2::labs(fill = legend, title = title) +
        ggplot2::theme_void() +
        ggplot2::scale_fill_gradientn(
          colors = my_palette,
          values = scales::rescale(c(min_val, center_val, max_val)),
          breaks = pretty(c(min_val, max_val), n = 5),
          labels = scales::label_number(accuracy = 0.01)
        )
    } else {
      my_palette <- GHRexplore::GHR_palette(palette, ncols = ifelse(centering == 0, 10, 3))(ifelse(centering == 0, 10, 3))
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = merged_map, ggplot2::aes(fill = median)) +
        ggplot2::labs(fill = legend, title = title) +
        ggplot2::theme_void()
      
      if (centering == 0) {
        min_val <- min(merged_map$median, na.rm = TRUE)
        max_val <- max(merged_map$median, na.rm = TRUE)
        center_val <- 0
        rescaled_center <- (center_val - min_val) / (max_val - min_val)
        my_palette <- GHRexplore::GHR_palette(palette)(n = 30)
        
        p <- p + ggplot2::scale_fill_gradientn(
          colors = my_palette,
          values = scales::rescale(c(min_val, center_val, max_val)),
          breaks = pretty(c(min_val, max_val), n = 5),
          labels = scales::label_number(accuracy = 0.01)
        )
        
      } else {
        # normalized the selected midpoint on a 0-1 scale 
        my_palette <- GHRexplore::GHR_palette(palette)(n = 25) # Odd number, 13 is the centre
        rescaled_center <- (centering - min(merged_map$median)) /
          (max(merged_map$median) - min(merged_map$median))
        values_pos <- c(seq(0, rescaled_center, length.out = 13)[1:12],
                        rescaled_center,
                        seq(rescaled_center, 1, length.out = 13)[2:13])
        p <- p + ggplot2::scale_fill_gradientn(
          colors = my_palette,
          values = values_pos) 
      }
    }
    
    # Apply facets to map plot if needed
    if (length(mod_id) > 1) {
      p <- p + ggplot2::facet_wrap(~ type, labeller = ggplot2::label_wrap_gen(width = 15))
    }
    if (replicated) {
      if (length(mod_id) == 1) {
        p <- p + ggplot2::facet_wrap(~ rep_label, dir = "h",
                                     labeller = ggplot2::label_wrap_gen(width = 15))
      } else if (length(mod_id) == 2) {
        p <- p + ggplot2::facet_wrap(~ rep_label + type, dir = "h",
                                     labeller = ggplot2::label_wrap_gen(width = 15))
      } else {
        stop("Plotting replicated maps comparing more than 2 models is not recommended.\nPlease plot maximum 2 models at a time when spatial random effects are replicated.")
      }
    }
  }
  
  return(p)
}
