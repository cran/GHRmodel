#' Plot Posterior Predictive Densities Versus Observed Data
#'
#' This function draws kernel-density curves for posterior-predictive samples
#' and observed data using \code{ggplot2::geom_line()}.  Each predictive
#' sample’s density is plotted in light blue; the observed density is overlaid
#' in black.
#'
#' @param ppd  A \code{data.frame} containing posterior-predictive samples
#'             (one column per sample) and the column with \code{observed} data.
#' @param xlab Character: x-axis label. Default \code{"Outcome"}.
#' @param ylab Character: y-axis label. Default \code{"Density"}.
#' @param title Character: plot title. Default
#'              \code{"Posterior Predictive Distribution"}.
#' @param xlim Numeric vector of length 2 giving the minimum and maximum
#'             x-axis values, e.g. \code{c(0, 25)}.  
#'             If \code{NULL} (default) the limits are
#'             \code{c(0, quantile(observed, 0.95))}.
#' @param obs_color Color for the observed line density 
#' @param ppd_color Color for the posterior predictive distribution lines density 

#' @return A \pkg{ggplot2} plot object.
#' 
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
#' 
#' # Plot densities of the posterior predictive distribution and observed cases.
#' plot_ppd(ppd_df, obs_color = "blue", ppd_color = "red")
#' }
#' 
#' @export
plot_ppd <- function(ppd,
                     xlab   = "Outcome",
                     ylab   = "Density",
                     title  = "Posterior Predictive Distribution",
                     xlim   = NULL,
                     obs_color = NULL,
                     ppd_color = NULL) {
  
  ## 1) Prepare observed data frame ----
  observed_df <- data.frame(
    value  = ppd$observed,
    type   = "Observed",
    sample = "Observed"
  )
  
  ## 2) Pivot predictive samples into long format ----
  predicted_df <- ppd |>
    dplyr::select(-observed)
  
  predicted_long <- tidyr::pivot_longer(
    data      = predicted_df,
    cols      = dplyr::everything(),
    names_to  = "sample",
    values_to = "value"
  )
  predicted_long$type <- "Predicted"
  
  ## 3) Handle x-axis limits ----

    if (is.null(xlim)) {
    xlim <- c(0,
              stats::quantile(observed_df$value,
                              probs = 0.95,
                              na.rm = TRUE))
  } else {
    if (!is.numeric(xlim) || length(xlim) != 2)
      stop("`xlim` must be a numeric vector of length 2, e.g. c(min, max).")
  }
  from_val <- xlim[1]
  to_val   <- xlim[2]
  
  ## 4) Compute kernel densities for each predictive sample ----

    dens_pred <- predicted_long |>
    dplyr::group_by(sample) |>
    dplyr::group_split() |>
    lapply(function(g) {
      d <- stats::density(g$value, from = from_val, to = to_val, n = 4096)
      data.frame(sample = unique(g$sample),
                 x      = d$x,
                 y      = d$y,
                 type   = "Predicted")
    }) |>
    dplyr::bind_rows()
  
  ## 5) Compute density for observed data ----

    d_obs <- stats::density(observed_df$value,
                          from = from_val, to = to_val, n = 4096)
  dens_obs <- data.frame(sample = "Observed",
                         x      = d_obs$x,
                         y      = d_obs$y,
                         type   = "Observed")
  
  ## 6) Plot ----
  
  obs_color <- ifelse(is.null(obs_color), "black" , obs_color)
  ppd_color <- ifelse(is.null(ppd_color), "#d4cbf5" , ppd_color)
  
    p <- ggplot2::ggplot(dens_pred,
                       ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(ggplot2::aes(group = sample,
                                    color = "PPD samples"),
                       linewidth = 0.3,
                       alpha     = 0.1) +
    ggplot2::geom_line(data = dens_obs,
                       ggplot2::aes(color = "Observed"),
                       linewidth = 0.5) +
    ggplot2::scale_color_manual(values = c("PPD samples"  = ppd_color,
                                           "Observed" = obs_color)) +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(alpha = 1, linewidth = 1))) +
    ggplot2::labs(color = "Density") +
    ggplot2::coord_cartesian(xlim = xlim, expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title)
  
  return(p)
}
