#' Plot `crosspred` Objects: Overall, Slices, or Heatmap
#'
#' Generate plots from a \code{"crosspred"} object. 
#' Three plot types are available:
#' \itemize{
#'   \item \code{type = "overall"}: Shows the overall exposure–response relationship, aggregated across all lags.
#'   \item \code{type = "slices"}: Produces line plots with credible interval ribbons, either 
#'         across lags (for a fixed \code{var}) or across values of \code{var} (for a fixed \code{lag}).
#'   \item \code{type = "heatmap"}: Displays a two-dimensional heatmap of effects across both 
#'         \code{var} and \code{lag}. Not applicable for one-basis models.
#' }
#'
#' @param crosspred An object of class \code{"crosspred"} or \code{"GHR_crosspred"},
#'  produced by \code{\link[dlnm]{crosspred}} or \code{\link[GHRmodel]{crosspred_inla}}.
#' @param type Character string. Options: \code{"overall"}, \code{"slices"}, or \code{"heatmap"}.
#' @param var Optional numeric vector of exposure values (used when \code{type = "slices"} to plot across lags).
#' @param lag Optional numeric vector of lag values (used when \code{type = "slices"} to plot across variables).
#' @param exp Logical. If \code{TRUE}, exponentiates the results (e.g., for log or logit links).
#' @param palette Character string for heatmap palette when \code{type = "heatmap"}. Options:
#'  `GHR`, `RColorBrewer` or `colorspace` palette (e.g. "Purp").
#' @param n_lag_smooth Integer, number of interpolation points along lag for heatmap smoothing (default = 50).
#' @param line_color Character string. Line color when \code{type = "slices"} or
#'  \code{type = "overall"}. Default is "black".
#' @param line_size Numeric. Line width (default = 0.7).
#' @param ribbon_color Character string. Color for credible interval ribbons.
#'  Defaults to \code{line_color}.
#' @param ribbon_alpha Numeric. Alpha transparency for ribbons (default = 0.2).
#' @param title  Character string. Plot title.
#' @param ylab  Character string. Label for y-axis.
#' @param xlab Character string. Label for x-axis.
#' @param ... Additional arguments passed to `ggplot2` functions.
#'
#' @return A \code{ggplot} object for the specified plot type.
#'
#' @seealso \code{\link[dlnm]{crosspred}}
#'
#' @import ggplot2
#' 
#' @examples
#' # Load example GHRmodels object from the package
#' model_dlnm_file <- system.file("examples", "model_dlnm.rds", package = "GHRmodel")
#' model_dlnm <- readRDS(model_dlnm_file)
#' 
#' # Load example cross-basis matrix from the package: 2-dimensional cross-basis matrix of the 
#' # non-linear effect of dengue risk across tmin values and lags: 
#' cb_tmin_file <- system.file("examples","cb_tmin.rds", package = "GHRmodel")
#' cb_tmin <- readRDS(cb_tmin_file) # loads cross-basis matrix into the environment
#'
#' # Generate predictions
#' pred_result <- crosspred_inla(
#'   models    = model_dlnm,
#'   basis    = cb_tmin,
#'   mod_id = "mod3",
#'   at       = seq(17, 24, by = 1),  # e.g., temperature sequence
#'   lag      = 2,
#'   cen      = 20,
#'   ci.level = 0.95
#' )
#'
#'
#' # Plot DLNM predictions 
#'plot_coef_crosspred(
#' crosspred = pred_result,    # Crosspred object with model predictions
#' type = "slices",            # Plot temperature-specific slices of exposure-response curves
#' exp = TRUE,                 # Exponentiate the coefficients (to relative risk scale)
#' var = c(22:24),             # Display results for temperature 22°C to 24°C
#' line_color = "red",         # Red color for the lines representing effect estimates
#' line_size = 0.8,            # Line thickness set to 0.8 for better visibility
#' ribbon_color = "red",       # Red shading for credible interval ribbons
#' ribbon_alpha = 0.3,         # Set ribbon transparency to 30%
#' title = "Effect of minimum temperatures 22°C to 23°C on dengue relative risk by lag",
#' xlab = "Lag",               # Label for the x-axis (exposure variable)
#' ylab = "Relative Risk (RR)" # Label for the y-axis (effect estimate scale)
#' )
#' 
#' @export
#' 
plot_coef_crosspred <- function(crosspred,
                                type = c("heatmap", "slices", "overall"),
                                var = NULL,
                                lag = NULL,
                                exp = FALSE,
                                palette = "-RdBu",
                                n_lag_smooth = 50,
                                line_color    = "black",
                                line_size     = 0.7,
                                ribbon_color  = NULL,
                                ribbon_alpha  = 0.2,
                                title = "",
                                ylab = NULL,
                                xlab = NULL,
                                ...) {
  
  # ---- Argument checks ----
  type <- match.arg(type) # ensure valid type
  if (!inherits(crosspred, "crosspred")) {
    stop("'crosspred' must be of class 'crosspred'.")
  }
  
  # ---- Helpers ----
  has_lag <- (diff(crosspred$lag) != 0)   # TRUE if there is a lag dimension
  do_exp  <- exp                          # whether to exponentiate results
  noeff   <- if (do_exp) 1 else 0         # reference line (1 for exp, 0 otherwise)
  if (is.null(ribbon_color)) ribbon_color <- line_color
  
  # ---- Case 1: "overall" effect plot ----
  if (type == "overall") {
    if (is.null(crosspred$allfit)) stop("No overall effect found in 'crosspred$allfit'.")
    
    df_over <- data.frame(
      Var  = crosspred$predvar,
      Fit  = if (do_exp) exp(crosspred$allfit) else crosspred$allfit,
      Low  = if (do_exp) exp(crosspred$alllow) else crosspred$alllow,
      High = if (do_exp) exp(crosspred$allhigh) else crosspred$allhigh
    )
    
    return(
      ggplot2::ggplot(df_over, ggplot2::aes(x = .data$Var, y = .data$Fit)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Low, ymax = .data$High),
                             fill = ribbon_color, alpha = ribbon_alpha, ...) +
        ggplot2::geom_line(color = line_color, linewidth = line_size, ...) +
        ggplot2::geom_hline(yintercept = noeff, color = "grey70", linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(label = title) +
        ggplot2::xlab(ifelse(is.null(xlab), "Var", xlab)) +
        ggplot2::ylab(ifelse(is.null(ylab), ifelse(do_exp, "exp(Effect)", "Effect"), ylab))
    )
  }
  
  # ---- Case 2: extract fit matrices for lagged / unlagged plots ----
  matfit  <- if (do_exp) exp(crosspred$matfit) else crosspred$matfit
  mathigh <- if (do_exp) exp(crosspred$mathigh) else crosspred$mathigh
  matlow  <- if (do_exp) exp(crosspred$matlow) else crosspred$matlow
  
  seqlg <- seq(crosspred$lag[1], crosspred$lag[2], by = crosspred$bylag) # sequence of lags
  predv <- crosspred$predvar                                              # predictor values
  
  # ---- Case 3: Heatmap (requires lag dimension) ----
  if (type == "heatmap") {
    if (!has_lag) stop("This crosspred object has no lag dimension.")
    
    # Interpolate fit values smoothly across lag axis
    lag_smooth <- seq(min(seqlg), max(seqlg), length.out = n_lag_smooth)
    df_heat <- expand.grid(Var = predv, Lag = lag_smooth)
    df_heat$Fit <- NA_real_
    
    for (i in seq_along(predv)) {
      row_name <- as.character(predv[i])
      df_heat$Fit[df_heat$Var == predv[i]] <-
        stats::approx(seqlg, matfit[row_name, ], xout = lag_smooth)$y
    }
    
    # Construct diverging palette centered at no-effect
    my_palette <- GHRexplore::GHR_palette(palette)(25)
    rescaled_center <- (noeff - min(df_heat$Fit)) / (max(df_heat$Fit) - min(df_heat$Fit))
    values_pos <- c(seq(0, rescaled_center, length.out = 13)[1:12],
                    rescaled_center,
                    seq(rescaled_center, 1, length.out = 13)[2:13])
    
    return(
      ggplot2::ggplot(df_heat, ggplot2::aes(x = .data$Var, y = .data$Lag, fill = .data$Fit)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradientn(colors = my_palette, values = values_pos) +
        ggplot2::theme_bw() + 
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::ggtitle(label = title) +
        ggplot2::xlab(ifelse(is.null(xlab), "Var", xlab)) +
        ggplot2::ylab(ifelse(is.null(ylab), "Lag", ylab)) +
        ggplot2::labs(fill = ifelse(do_exp, "RR", "Effect"))
    )
  }
  
  # ---- Case 4: Line plots (1D or slices) ----
  if (!has_lag && !is.null(lag)) stop("Lag slicing not allowed: no lag dimension present.")
  if (is.null(var) && is.null(lag)) stop("Specify either 'var' or 'lag'.")
  if (!is.null(var) && !is.null(lag)) stop("Provide only one of 'var' or 'lag', not both.")
  
  # ---- Subcase 4a: One-basis (no lag) ----
  if (!has_lag) {
    df_1d <- data.frame(
      Var  = as.numeric(rownames(matfit)),
      Fit  = matfit[, 1],
      Low  = matlow[, 1],
      High = mathigh[, 1]
    )
    
    # subset df_1d if 'var' is provided
    if (!is.null(var)) {
      df_1d <- df_1d[df_1d$Var %in% var, , drop = FALSE]
      if (nrow(df_1d) == 0) stop("None of the requested 'var' values found in crosspred$predvar.")
    }
    
    return(
      ggplot2::ggplot(df_1d, ggplot2::aes(x = .data$Var, y = .data$Fit)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Low, ymax = .data$High),
                             fill = ribbon_color, alpha = ribbon_alpha) +
        ggplot2::geom_line(color = line_color, linewidth = line_size) +
        ggplot2::geom_hline(yintercept = noeff, color = "grey70", linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(label = title) +
        ggplot2::xlab(ifelse(is.null(xlab), "Var", xlab)) +
        ggplot2::ylab(ifelse(is.null(ylab), "Effect", ylab))
    )
  }
  
  # ---- Subcase 4b: Lagged, slicing by predictor values ----
  if (!is.null(var)) {
    if (any(!var %in% predv)) stop("Some 'var' values not found in crosspred$predvar.")
    slices <- lapply(var, function(vv) {
      rn <- as.character(vv)
      data.frame(
        Var = vv,
        Lag = seqlg,
        Fit = matfit[rn, ],
        Low = matlow[rn, ],
        High = mathigh[rn, ]
      )
    })
    df <- do.call(rbind, slices)
    
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$Lag, y = .data$Fit)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Low, ymax = .data$High),
                             fill = ribbon_color, alpha = ribbon_alpha, ...) +
        ggplot2::geom_line(color = line_color, linewidth = line_size, ...) +
        ggplot2::facet_wrap(~.data$Var) +
        ggplot2::geom_hline(yintercept = noeff, color = "grey70", linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(label = title) +
        ggplot2::xlab(ifelse(is.null(xlab), "Lag", xlab)) +
        ggplot2::ylab(ifelse(is.null(ylab), "Effect", ylab))
    )
  }
  
  # ---- Subcase 4c: Lagged, slicing by lag values ----
  if (!is.null(lag)) {
    if (any(!lag %in% seqlg)) stop("Some 'lag' values not valid.")
    slices <- lapply(lag, function(ll) {
      col_idx <- which(seqlg == ll)
      data.frame(
        Lag = ll,
        Var = predv,
        Fit = matfit[, col_idx],
        Low = matlow[, col_idx],
        High = mathigh[, col_idx]
      )
    })
    df <- do.call(rbind, slices)
    
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$Var, y = .data$Fit)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Low, ymax = .data$High),
                             fill = ribbon_color, alpha = ribbon_alpha, ...) +
        ggplot2::geom_line(color = line_color, linewidth = line_size, ...) +
        ggplot2::facet_wrap(~.data$Lag) +
        ggplot2::geom_hline(yintercept = noeff, color = "grey70", linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(label = title) +
        ggplot2::xlab(ifelse(is.null(xlab), "Var", xlab)) +
        ggplot2::ylab(ifelse(is.null(ylab), "Effect", ylab))
    )
  }
  
  stop("Unexpected condition in 'plot_coef_crosspred'.")
}


