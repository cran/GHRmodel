#' Rank models by goodness-of-fit metrics
#'
#' Computes per-metric ranks and an average rank across selected
#' goodness-of-fit (GOF) metrics for a set of fitted models.
#' The original order of the input data is preserved.
#'
#' @param mod_gof A data.frame containing goodness-of-fit metrics for one or
#'   more models. Must include the columns specified in \code{metric}.
#' @param metric Character vector of column names in \code{mod_gof} giving
#'   the goodness-of-fit metrics to be ranked. All columns must be numeric;
#'   lower values are considered better. Duplicated names are ignored.
#'
#' @return
#' A data.frame with the same rows as \code{mod_gof}, augmented with:
#' \itemize{
#'   \item \code{rank_<metric>}: rank of each model for each GOF metric
#'   \item \code{rank_mean}: mean rank across all selected metrics, rounded to
#'   two decimal places
#'   \item \code{final_rank}: final ranking of models based on
#'   \code{rank_mean}, with ties broken using \code{ties.method = "first"}
#' }
#' The output is returned in the original row order of \code{mod_gof}.
#' Existing columns with the same names as the added columns are overwritten.
#'
#' @details
#' Ranking is performed independently for each metric in ascending order
#' (best fit = lowest value). Models with tied metric values share the same
#' rank (the average of the tied positions, i.e.
#' \code{ties.method = "average"}), so results do not depend on row order or
#' on the order of \code{metric}. The average rank
#' (\code{rank_mean}) is computed as the row-wise mean of all metric-specific
#' ranks and rounded to two decimal places.
#'
#' Ties in \code{rank_mean} are resolved using \code{\link[base]{rank}} with
#' \code{ties.method = "first"}, so tied models are ordered by their position
#' in the input, ensuring a deterministic ordering.
#'
#' Missing values in a metric are ranked last
#' (see \code{\link[base]{rank}}).
#'
#' @examples
#' \donttest{
#' # Load example GHRmodels object from the package:
#' model_list_file <- system.file("examples", "model_list.rds",
#'                                package = "GHRmodel")
#' model_list <- readRDS(model_list_file)
#' .rank_gof(model_list$mod_gof, metric = c("waic", "lms"))
#' }
#'
#' @noRd

.rank_gof <- function(mod_gof, metric) {
  # Input checks
  if (!is.data.frame(mod_gof)) {
    stop(
      "The argument 'mod_gof' must be a data.frame. ",
      "Please input 'model$mod_gof'."
    )
  }
  if (!is.character(metric) || length(metric) == 0 || anyNA(metric)) {
    stop("'metric' must be a non-empty character vector of column names.")
  }
  metric <- unique(metric)
  if (!all(metric %in% names(mod_gof))) {
    stop(
      "All metrics in 'metric' must be present as columns ",
      "in the 'mod_gof' object."
    )
  }
  non_numeric <- metric[!vapply(mod_gof[metric], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      "All metrics in 'metric' must be numeric columns. Non-numeric: ",
      paste(non_numeric, collapse = ", "),
      "."
    )
  }

  df <- mod_gof

  # Create rank columns
  for (m in metric) {
    # Tied values share the same (average) rank, independently of row order
    df <- df |>
      dplyr::mutate(
        !!paste0("rank_", m) := rank(.data[[m]], ties.method = "average")
      )
  }

  # Average rank across metrics
  rank_cols <- paste0("rank_", metric)

  df <- df |>
    dplyr::mutate(
      rank_mean = round(
        rowMeans(
          dplyr::pick(
            dplyr::all_of(rank_cols)
          )
        ),
        digits = 2
      )
    )

  df <- df |>
    dplyr::mutate(
      final_rank = rank(.data$rank_mean, ties.method = "first")
    )

  return(df)
}
