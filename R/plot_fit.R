#' @title Plot Observed vs. Fitted Cases
#'
#' @description
#' This function creates a time-series plot comparing observed cases with fitted values from one or more models
#' in a \code{GHRmodels} object. The plot supports faceting by model and/or group.
#'
#' @param models A \code{GHRmodels} object containing fitted model output.
#' @param mod_id Character vector of model identifiers (from \code{models$mod_gof$model_id}) to plot.
#' @param time Character; name of the time-variable column in \code{models$data}.
#' @param group Optional; character name of the column defining independent time series (e.g., spatial areas).
#' @param group_id Optional vector of specific group values to subset if \code{group} is provided.
#' @param mod_label Optional custom labels for each model. Can be a named vector (e.g., \code{c("mod1" = "Base")})
#' or an unnamed vector with the same length and order as \code{mod_id}.
#' @param palette Character; name of the color palette for fitted lines. Default is \code{"IDE2"}.
#' @param ref_color Optional color to override the first model's line (reference model).
#' @param obs_color Color for observed data line. Default is \code{"black"}.
#' @param obs_label Legend label for observed data. Default is \code{"Observed"}.
#' @param title Character; title of the plot.
#' @param mod_facet Logical; if \code{TRUE}, faceting is applied by model. Can be combined with \code{group}.
#' @param ci Logical; if \code{TRUE}, adds 95% credible interval ribbons for model fits.
#' @param transform Character string for y-axis transformation. Defaults to \code{"identity"} (no transform).
#' Other options include \code{"log10p1"}, \code{"log1p"}, \code{"sqrt"}, etc.
#' @param ylab Label for the y-axis. Default is \code{"Cases"}.
#' @param xlab Label for the x-axis. Default is \code{"Time"}.
#' @param xlim Character vector of length two in "yyyy-mm-dd" format (e.g., \code{c("2010-01-01", "2020-12-31")}).
#' Use \code{NA} to leave one side open (e.g., \code{c("2015-01-01", NA)}).
#' @param legend Legend title for model lines. Default is \code{"Model"}.
#'
#' @return A \code{ggplot2} object:
#' \itemize{
#'   \item Time-series line plot of observed vs fitted cases
#'   \item Optionally includes credible intervals and facets by model or group
#'   \item X-axis can be limited by \code{xlim}; Y-axis can be transformed for readability
#' }
#'
#' @details
#' \itemize{
#'   \item Faceting is flexible: if \code{mod_facet = TRUE} and \code{group} is provided, both are used.
#'   \item If \code{ci = TRUE}, ribbons are plotted for fitted model uncertainty.
#'   \item \code{mod_label}, \code{ref_color}, and \code{obs_color} allow full customization of the legend.
#'   \item The function automatically sums values across replicates for grouped time series.
#' }
#'
#' @seealso 
#' \code{\link{fit_models}} to generate GHRmodels.
#' 
#' @importFrom stats setNames median
#' 
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package: 
#' model_list_file <- system.file("examples", "model_list.rds", package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Plot observed vs. fitted cases over time for three selected models
#' plot_fit(
#'   models = model_list,                         # A GHRmodels object containing the fitted models
#'   mod_id = c("mod1", "mod3", "mod5"),          # Vector of model IDs to plot
#'   mod_label = c("Baseline",                    # Custom display names 
#'                 "tmin.l1.nl",                  
#'                 "pdsi.l1.nl_tmin.l1.nl"),            
#'   ref_color = "grey",                          # Color for the reference model 
#'   time = "date",                               # Name of the time variable 
#'   palette = "Set2",                            # Color palette for fitted lines
#'   xlim = c("2010-01-01", "2020-01-01"),        # Limit x-axis to this date range
#'   title = "Fitted vs Observed"                 # Main plot title
#' )  
#' }
#'
#' @name plot_fit
#' @rdname plot_fit
#' @export


# ---- plot_fit function with comments and numeric mod_id ordering ----
plot_fit <- function(models = NULL,
                     mod_id = NULL,
                     time = NULL,
                     group = NULL,
                     group_id = NULL,
                     mod_label = NULL,
                     mod_facet = FALSE,
                     palette = "IDE2",
                     ref_color = NULL,
                     obs_color = NULL,
                     obs_label = NULL,
                     title = "",
                     ci = FALSE,
                     transform = "identity",
                     xlab = "Time",
                     ylab = "Cases",
                     xlim = NULL,
                     legend = "Model") {
  
  # ---- Helper to check date format ----
  date_chk <- function(d){
    if (is.na(d) || d == "") return(NA)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", d))
      stop("Date values in 'xlim' must be in 'yyyy-mm-dd' format", call. = FALSE)
    out <- as.Date(d, "%Y-%m-%d")
    if (is.na(out))
      stop("Unable to convert '", d, "' to Date", call. = FALSE)
    out
  }
  
  # ---- Basic argument checks ----
  if (!inherits(models, "GHRmodels"))
    stop("`models` must be of class 'GHRmodels'.", call. = FALSE)
  
  if (is.null(time) || !(time %in% names(models$data)))
    stop("Valid `time` column must be supplied.", call. = FALSE)
  
  if (!is.null(group) && !(group %in% names(models$data)))
    stop(paste0("`group` column '", group, "' not found in models data."), call. = FALSE)
  
  if (!is.null(group_id) && is.null(group))
    stop("`group_id` provided but `group` is NULL.", call. = FALSE)
  
  if (!is.null(group_id)){
    bad_gid <- setdiff(group_id, unique(models$data[[group]]))
    if (length(bad_gid))
      stop("Invalid `group_id` values: ", paste(bad_gid, collapse = ", "), call. = FALSE)
  }
  
  if (is.null(mod_id))
    mod_id <- models$mod_gof$model_id
  
  bad_mods <- setdiff(mod_id, models$mod_gof$model_id)
  if (length(bad_mods))
    stop("Unknown mod_id(s): ", paste(bad_mods, collapse = ", "), call. = FALSE)
  
  # ---- Process mod_label ----
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
  
  # ---- Validate transformation argument ----
  val_trans <- c("log10p1", "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", 
                 "log", "log10", "log1p", "log2", "logit", "modulus", "probability", 
                 "probit", "pseudo_log", "reciprocal", "reverse", "sqrt", "time")
  if(!transform %in% val_trans){
    stop("Invalid transform.") 
  }
  
  # ---- Validate xlim ----
  if (!is.null(xlim)){
    if (length(xlim) != 2)
      stop("`xlim` must be a vector of length 2 (lower, upper)", call. = FALSE)
    xmin_date <- date_chk(xlim[1])
    xmax_date <- date_chk(xlim[2])
    if (!is.na(xmin_date) && !is.na(xmax_date) && xmin_date > xmax_date)
      stop("Lower bound in `xlim` cannot be later than upper bound", call. = FALSE)
  } else {
    xmin_date <- xmax_date <- NA
  }
  
  # ---- Aggregate model data ----
  df_list <- lapply(mod_id, function(i){
    base <- data.frame(
      model_id = i,
      cases    = models$data[[models$outcome[[1]]]],
      time     = models$data[[time]],
      mean_fit = models$fitted[[i]]$mean,
      lci_fit  = models$fitted[[i]]$lci,
      uci_fit  = models$fitted[[i]]$uci
    )
    
    base$group <- if (is.null(group)) 1 else models$data[[group]]
    if (!is.null(group_id))
      base <- dplyr::filter(base, .data$group %in% group_id)
    
    base |>
      dplyr::group_by(time, group, model_id) |>
      dplyr::summarise(
        cases    = sum(cases, na.rm = TRUE),
        mean_fit = sum(mean_fit),
        lci_fit  = sum(lci_fit),
        uci_fit  = sum(uci_fit),
        .groups  = "drop"
      ) |>
      dplyr::mutate(time = as.Date(time))
  })
  df <- dplyr::bind_rows(df_list)
  # ---- Fix factor level order for correct legend ----
  df$model_id <- factor(df$model_id, levels = mod_id)
  
  # ---- Apply xlim cropping ----
  if (!is.na(xmin_date)) df <- dplyr::filter(df, .data$time >= xmin_date)
  if (!is.na(xmax_date)) df <- dplyr::filter(df, .data$time <= xmax_date)
  
  # ---- Facet label prep ----
  if (mod_facet) {
    df$model_id_label <- factor(df$model_id, levels = mod_id, labels = unname(mod_label[mod_id]))
  }
  
  # ---- Color and label mapping ----
  if (is.null(obs_color)) obs_color <- "black"
  if (is.null(obs_label)) obs_label <- "Observed"
  
  model_ids <- mod_id  # enforce sorted order
  legend_keys <- c(model_ids, "Observed")
  
  pal <- rev(GHRexplore::GHR_palette(palette,length(model_ids))(length(model_ids)))
  colours <- stats::setNames(pal, model_ids)
  if (!is.null(ref_color)) colours[model_ids[1]] <- ref_color
  colours["Observed"] <- obs_color
  
  labels <- stats::setNames(c(unname(mod_label[model_ids]), obs_label), legend_keys)
  
  # ---- Build base plot ----
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = mean_fit, colour = model_id), alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = cases, colour = "Observed"), alpha = 0.8) +
    ggplot2::scale_colour_manual(name = legend, values = colours, labels = labels) +
    ggplot2::labs(x = xlab, y = ylab, colour = legend, fill = legend) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  # ---- Add CI ribbons ----
  if (ci){
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lci_fit, ymax = uci_fit, fill = model_id),
                                  alpha = 0.3, show.legend = FALSE) +
      ggplot2::scale_fill_manual(name = legend, values = colours, labels = labels)
  }
  
  # ---- Y-axis transformation ----
  if(transform == "log10p1"){
    if(ci == FALSE) {
      p <- p + ggplot2::scale_y_continuous(transform = .log10p1_trans,
                                           breaks =  .log10_breaks_like(c(0,p$data$mean_fit)),
                                           labels =  .log10_breaks_like(c(0,p$data$mean_fit))) 
    } else {
      p <- p + ggplot2::scale_y_continuous(transform = .log10p1_trans,
                                           breaks =  .log10_breaks_like(c(0,p$data$uci_fit)),
                                           labels =  .log10_breaks_like(c(0,p$data$uci_fit))) 
    }
  } else { 
    p <- p + ggplot2::scale_y_continuous(transform = transform)
  }
  
  # ---- X-axis scale ----
  if (!is.na(xmin_date) || !is.na(xmax_date)){
    p <- p + ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                                   limits = c(xmin_date, xmax_date))
  } else {
    p <- p + ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  }
  
  # ---- Faceting logic ----
  if (!is.null(group) && mod_facet){
    p <- p + ggplot2::facet_grid(model_id_label ~ group, scales = "free_y",
                                 labeller = ggplot2::label_wrap_gen(15)) 
  } else if (!is.null(group)){
    p <- p + ggplot2::facet_wrap(. ~ group, scales = "free_y",
                                 labeller = ggplot2::label_wrap_gen(15)) 
  } else if (mod_facet){
    p <- p + ggplot2::facet_wrap(~ model_id_label, scales = "free_y",
                                 labeller = ggplot2::label_wrap_gen(15)) 
  }
  
  return(p)
}


