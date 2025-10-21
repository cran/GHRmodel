#' Generate lagged variables for one or more lags
#'
#' This function creates lagged versions of one or more numeric or categorical variables 
#' in an equally spaced time-series data set. A single call can create multiple lags
#' for each selected variable and, optionally, for each spatial/grouping unit.
#'
#' @param data A \code{data.frame} containing equally spaced observations.
#' @param name  A character vector: name of the variable (or variables) to lag.
#' @param time A single character string: name of the time-index variable (e.g.,
#'   \code{"date"}).
#' @param lag  A numeric vector of one or more positive integers. Each value is
#'   interpreted as a 'lag' (i.e. shift the series backward by \eqn{k} observations).
#' @param group Optional character vector naming column(s) that define
#'   independent time-series (e.g. regions). If \code{NULL}, the whole data set
#'   is treated as one series.
#' @param add Logical. If \code{TRUE} (default) the lagged columns are appended
#'   to \code{data}; if \code{FALSE} the function returns only the lagged
#'   columns as a matrix.
#'
#' @return Either a data frame (when \code{add = TRUE}) containing the original
#'   data plus the new lagged columns, or a numeric matrix of lagged values
#'   (when \code{add = FALSE}).
#'
#' @examples
#' 
#' ## Daily series for two micro-regions
#' d <- data.frame(
#'   date       = as.Date("2023-01-01") + 0:9,
#'   micro_code = rep(c("A", "B"), each = 5),
#'   tmin       = rnorm(10, 10, 2),
#'   pdsi       = rnorm(10)
#' )
#'
#' ## Create lags 1 to 3 for tmin and pdsi
#' lagged <- lag_cov(
#'   data  = d,
#'   name   = c("tmin", "pdsi"),
#'   time  = "date",
#'   group = "micro_code",
#'   lag   = c(1:3)
#' )
#'
#' ## Only lagged columns (matrix),
#' lag_only <- lag_cov(
#'   data = d, name = "tmin", time = "date",
#'   lag  = c(1:3), add = FALSE
#' )
#' @export
#' 
lag_cov <- function(data,
                    name,
                    time,
                    lag,
                    group = NULL,
                    add   = TRUE) {
  
  ## 1) Checks 
  if (missing(data) || missing(name) || missing(time) || missing(lag)) {
    missing_args <- c()
    if (missing(data))  missing_args <- c(missing_args, "data")
    if (missing(name))   missing_args <- c(missing_args, "name")
    if (missing(time))  missing_args <- c(missing_args, "time")
    if (missing(lag))   missing_args <- c(missing_args, "lag")
    stop("Missing required argument(s): ", paste(missing_args, collapse = ", "))
  }
  
  if (!is.character(name))
    stop("'name' must be a character vector.")
  
  if (!is.character(time) || length(time) != 1)
    stop("'time' must be a single character string.")
  
  # `lag` must be a numeric vector ---------------------------
  if (!is.numeric(lag) || any(lag < 0) || any(lag %% 1 != 0)) {
    stop("'lag' must be a vector of positive integers.")
  }
  lag <- sort(unique(as.integer(lag)))  # ensure sorted, unique integers
  
  if (!all(c(name, time) %in% names(data))) {
    missing_vars <- c(name, time)[!(c(name, time) %in% names(data))]
    stop("The following 'name' or 'time' are not present in the dataset: ",
         paste(missing_vars, collapse = ", "))
  }
  if (!is.null(group) && !all(group %in% names(data))) {
    missing_groups <- group[!(group %in% names(data))]
    stop("The following 'group' variables are not present in the dataset: ",
         paste(missing_groups, collapse = ", "))
  }
  if (any(is.na(data[[time]])))
    stop("The 'time' column contains missing values. Please clean it before lagging.")
  
  if (!.check_consecutive(data, time, group))
    stop("Non-consecutive time series detected within at least one group.")
  
  ## 2) ordering 
  if (!is.null(group)) {
    data <- data |>
      dplyr::group_by(dplyr::across(tidyr::all_of(group))) |>
      dplyr::arrange(dplyr::across(tidyr::all_of(c(group, time))))
  } else {
    data <- dplyr::arrange(data, .data[[time]])
  }
  
  ## 3) create lagged columns 
  lag_data <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyr::all_of(name),
        .fns  = stats::setNames(
          lapply(lag, function(k) function(x) dplyr::lag(x, n = k)),
          paste0("l", lag)
        ),
        .names = "{.col}.{.fn}"
      )
    ) |>
    dplyr::ungroup()
  
  ## 4) drop originals if add = FALSE 
  if (!add) {
    lag_cols <- paste0(rep(name, each = length(lag)),
                       ".l",
                       rep(lag, times = length(name)))
    if (!all(lag_cols %in% names(lag_data)))
      stop("Some lagged columns were not created as expected.")
    
    lag_data <- lag_data |>
      dplyr::select(tidyr::all_of(lag_cols)) |>
      as.matrix()
  }
  
  return(lag_data)
}
