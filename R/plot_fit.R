#' @title Plot Observed vs. Fitted Cases
#'
#' @description
#' This function creates a time series plot comparing observed cases with fitted
#' values (i.e., mean predicted counts) from one or more models in a
#' \code{GHRmodels} object.
#'
#' @param models A \code{GHRmodels} object containing fitted model output.
#' @param mod_id Character vector of model identifiers (from
#' \code{models$mod_gof$model_id}) to plot.
#' @param time Character; name of the time variable column in
#' \code{models$data}. Its values must be in date format ("yyyy-mm-dd")
#' representing the day of observation for daily data, the first day of the
#' week for weekly, or the first day of the month for monthly observations.
#' @param unit Optional; character name of the column in
#' \code{models$data} that identifies the different units for
#' which a time series is available, typically a spatial unit.
#' @param unit_id Optional character vector of specific unit values to subset if
#' \code{unit} is provided. Cannot be used if \code{aggregate_space} is also used.
#' @param ci Logical; if \code{TRUE}, adds 95% credible interval ribbons
#' for model fits. Can only be used if \code{aggregate_space} is NULL.
#' @param aggregate_space Name of variable used to define spatial
#' aggregation groups that contain the different units. Counts within
#' that aggregation group will be summed. Use \code{"all"} to aggregate
#' all spatial units. Cannot be used with models using a gaussian family.
#' @param mod_label Optional custom labels for each model. Can be a named
#' vector (e.g., \code{c("mod1" = "Base")}) or an unnamed vector with the
#' same length and order as \code{mod_id}.
#' @param palette Character; name of the color palette for fitted lines.
#' Default is \code{"IDE2"}.
#' @param ref_color Optional color to override the first model's line
#' (reference model).
#' @param obs_color Color for observed data line. Default is \code{"black"}.
#' @param obs_label Legend label for observed data. Default is
#' \code{"Observed"}.
#' @param title Character; title of the plot.
#' @param transform Character string for y-axis transformation. Defaults
#' to \code{"identity"} (no transform). Other options include
#' \code{"log10p1"}, \code{"log1p"}, \code{"sqrt"}, etc.
#' @param ylab Label for the y-axis. Default is \code{"Cases"}.
#' @param xlab Label for the x-axis. Default is \code{"Time"}.
#' @param xlim Date vector of length two (e.g., \code{as.Date(c("2010-01-01",
#' "2020-12-31"))}). Use \code{NA} to leave one side open (e.g.,
#' \code{as.Date(c("2015-01-01", NA))}).
#' @param legend Legend title for model lines. Default is \code{"Model"}.
#'
#' @return A \code{ggplot2} object with the time series line plot of observed
#'  vs fitted cases.
#'
#' @seealso
#' \code{\link{fit_models}} to generate GHRmodels.
#'
#' @importFrom stats setNames median
#'
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package:
#' model_list_file <- system.file("examples", "model_list.rds",
#'                                package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#'
#' # Plot observed vs. fitted cases over time for three selected models
#' plot_fit(
#'   models = model_list,
#'   mod_id = c("mod1", "mod3", "mod5"),
#'   mod_label = c("Baseline", "tmin.l1.nl", "pdsi.l1.nl_tmin.l1.nl"),
#'   time = "date",
#'   unit = "micro_name",
#'   xlim = as.Date(c("2015-01-01", NA))
#' )
#' }
#'
#' @export

# ---- plot_fit function with comments and numeric mod_id ordering ----
plot_fit <- function(
  models,
  mod_id = NULL,
  time = NULL,
  unit = NULL,
  unit_id = NULL,
  ci = FALSE,
  aggregate_space = NULL,
  mod_label = NULL,
  palette = "IDE2",
  ref_color = NULL,
  obs_color = NULL,
  obs_label = NULL,
  title = "",
  transform = "identity",
  xlab = "Time",
  ylab = "Cases",
  xlim = NULL,
  legend = "Model"
) {
  # ---- Helper to check date format ----

  # ---- Basic argument checks ----
  if (!inherits(models, "GHRmodels")) {
    stop("`models` must be of class 'GHRmodels'.", call. = FALSE)
  }
  mod_data <- models$data
  mod_fitted <- models$fitted

  # Check time exists, is in the data.frame and is in date format
  if (missing(time)) {
    stop("Error: Missing required argument 'time'.")
  } else if (is.null(mod_data[[time]])) {
    stop("'time' not found in the data")
  } else if (anyNA(.ymd_strict(mod_data[[time]]))) {
    stop("'Date' should be in 'yyyy-mm-dd' format.")
  }

  # Check if 'unit' exists in data, also check unit_id
  if (!is.null(unit) && is.null(mod_data[[unit]])) {
    stop("No column of the data matches the 'unit' argument.")
  }
  if (!is.null(unit_id) && is.null(unit)) {
    stop("'unit_id' provided but 'unit' is NULL.")
  }
  if (!is.null(unit_id) && !is.null(aggregate_space)) {
    stop("'unit_id' cannot be used if 'aggregate_space' is not NULL.")
  }
  if (!is.null(unit_id)) {
    if (length(unit_id) == 0) {
      stop("'unit_id' must contain at least one value.")
    }
    bad_units <- setdiff(unit_id, unique(mod_data[[unit]]))
    if (length(bad_units)) {
      stop(
        "Invalid 'unit_id' values: ",
        paste(bad_units, collapse = ", ")
      )
    }
  }

  # check duplicated time points
  GHRexplore::check_consecutive(mod_data, time, unit, strict = FALSE)

  # Check that 'aggregate_space' is valid if specified; family not gaussian
  if (
    !is.null(aggregate_space) &&
      is.null(mod_data[[aggregate_space]]) &&
      aggregate_space != "all"
  ) {
    stop("No column of the data match the 'aggregate_space' argument.")
  }
  if (!is.null(aggregate_space) && any(models$family == "gaussian")) {
    stop(
      "'aggregate_space' cannot be used with models with a gaussian likelihood."
    )
  }

  # Check that if 'aggregate_space' is valid unit is also specified
  if (!is.null(aggregate_space) && is.null(unit)) {
    stop("No 'unit' argument provided.")
  }

  # Assign default mod_id, check if correct
  if (is.null(mod_id)) {
    mod_id <- models$mod_gof$model_id
  }
  bad_mods <- setdiff(mod_id, models$mod_gof$model_id)
  if (length(bad_mods)) {
    stop("Unknown mod_id(s): ", paste(bad_mods, collapse = ", "), call. = FALSE)
  }

  # ci only if not aggregate space
  if (isTRUE(ci) && !is.null(aggregate_space)) {
    stop("'ci' are not available when using 'aggregate_space'.")
  }

  # Process mod_labels
  if (!is.null(mod_label)) {
    if (!is.null(names(mod_label))) {
      full_label <- setNames(mod_id, mod_id)
      full_label[names(mod_label)] <- mod_label
      mod_label <- full_label
    } else {
      if (length(mod_label) != length(mod_id)) {
        stop(
          "`mod_label` must be a named vector or have ",
          "the same length as `mod_id`.",
          call. = FALSE
        )
      }
      names(mod_label) <- mod_id
    }
  } else {
    mod_label <- stats::setNames(mod_id, mod_id)
  }

  # Validate transformation argument
  .check_transform(transform)

  # Validate xlim
  if (!is.null(xlim)) {
    if (!inherits(xlim, "Date")) {
      stop("'xlim' must be of type date.")
    }
    if (length(xlim) != 2) {
      stop("`xlim` must be a vector of length 2 (lower, upper)", call. = FALSE)
    }
    xmin_date <- xlim[1]
    xmax_date <- xlim[2]
    if (!is.na(xmin_date) && !is.na(xmax_date) && xmin_date > xmax_date) {
      stop(
        "Lower bound in `xlim` cannot be later than upper bound",
        call. = FALSE
      )
    }
  } else {
    xmin_date <- xmax_date <- NA
  }

  # Aggregate model data if needed, otherwise just fetch data
  if (!is.null(aggregate_space)) {
    if (aggregate_space == "all") {
      aggr_var <- "All units"
    } else {
      aggr_var <- mod_data[[aggregate_space]]
    }
    df_list <- lapply(mod_id, function(i) {
      base <- data.frame(
        model_id = i,
        cases = mod_data[[models$outcome[[1]]]],
        time = mod_data[[time]],
        unit = aggr_var,
        mean_fit = mod_fitted[[i]]$mean,
        lci_fit = mod_fitted[[i]]$lci,
        uci_fit = mod_fitted[[i]]$uci
      )

      base |>
        dplyr::group_by(time, unit, model_id) |>
        dplyr::summarise(
          cases = sum(cases, na.rm = TRUE),
          mean_fit = sum(mean_fit),
          .groups = "drop"
        ) |>
        dplyr::mutate(time = as.Date(time))
    })
  } else {
    df_list <- lapply(mod_id, function(i) {
      base <- data.frame(
        model_id = i,
        cases = mod_data[[models$outcome[[1]]]],
        time = mod_data[[time]],
        mean_fit = mod_fitted[[i]]$mean,
        lci_fit = mod_fitted[[i]]$lci,
        uci_fit = mod_fitted[[i]]$uci
      )
      if (!is.null(unit)) {
        base[["unit"]] <- mod_data[[unit]]
      }
      # Filter to selected unit if needed
      if (!is.null(unit_id)) {
        base <- dplyr::filter(base, .data$unit %in% unit_id)
      }
      base
    })
  }
  df <- dplyr::bind_rows(df_list)

  # ---- Fix factor level order for correct legend ----
  df$model_id <- factor(df$model_id, levels = mod_id)

  # ---- Apply xlim cropping ----
  if (!is.na(xmin_date)) {
    df <- dplyr::filter(df, .data$time >= xmin_date)
  }
  if (!is.na(xmax_date)) {
    df <- dplyr::filter(df, .data$time <= xmax_date)
  }

  # ---- Color and label mapping ----
  if (is.null(obs_color)) {
    obs_color <- "black"
  }
  if (is.null(obs_label)) {
    obs_label <- "Observed"
  }

  model_ids <- mod_id # enforce sorted order

  pal <- rev(GHRexplore::GHR_palette(palette, length(model_ids))(length(
    model_ids
  )))
  colours <- stats::setNames(pal, model_ids)
  if (!is.null(ref_color)) {
    colours[model_ids[1]] <- ref_color
  }

  labels <- stats::setNames(unname(mod_label[model_ids]), model_ids)

  # ---- Build base plot ----
  # Observed data is not a model, so it gets its own legend (linetype scale)
  # placed above the model legend (colour scale).
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_fit, colour = model_id),
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = cases, linetype = "Observed"),
      colour = obs_color,
      alpha = 0.8
    ) +
    ggplot2::scale_colour_manual(
      name = legend,
      values = colours,
      labels = labels
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("Observed" = "solid"),
      labels = c("Observed" = obs_label)
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        order = 1,
        override.aes = list(colour = obs_color)
      ),
      colour = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::labs(x = xlab, y = ylab, colour = legend) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  # ---- Add CI ribbons ----
  if (ci) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lci_fit, ymax = uci_fit, fill = model_id),
        alpha = 0.3,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        name = legend,
        values = colours,
        labels = labels
      )
  }

  # ---- Y-axis transformation ----
  if (transform == "log10p1") {
    if (ci == FALSE) {
      p <- p +
        ggplot2::scale_y_continuous(
          transform = GHRexplore::log10p1_trans,
          breaks = GHRexplore::log10p1_breaks_like(c(0, p$data$mean_fit)),
          labels = GHRexplore::log10p1_breaks_like(c(0, p$data$mean_fit))
        )
    } else {
      p <- p +
        ggplot2::scale_y_continuous(
          transform = GHRexplore::log10p1_trans,
          breaks = GHRexplore::log10p1_breaks_like(c(0, p$data$uci_fit)),
          labels = GHRexplore::log10p1_breaks_like(c(0, p$data$uci_fit))
        )
    }
  } else {
    p <- p + ggplot2::scale_y_continuous(transform = transform)
  }

  # ---- X-axis scale ----
  if (!is.na(xmin_date) || !is.na(xmax_date)) {
    p <- p +
      ggplot2::scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y-%m-%d",
        limits = c(xmin_date, xmax_date)
      )
  } else {
    p <- p +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m-%d")
  }

  # ---- Faceting logic ----
  if (!is.null(unit)) {
    p <- p +
      ggplot2::facet_wrap(
        . ~ unit,
        scales = "free_y"
      )
  }

  return(p)
}
