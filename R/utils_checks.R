#_______________________________________________________________________________
# Helper Functions for Internal Checks ----------------------------------------
#_______________________________________________________________________________

# get_time interval -----------------------------------------------------------

#' Extract Time Interval
#' 
#' Determines the time interval (e.g., daily, weekly, monthly, yearly)' of a 
#' dataset based on the differences across sequential time points.
#'
#' @param data A data frame containing equally spaced time series data (daily, weekly, monthly, yearly)
#'             for one or multiple spatial units or groups.             
#' @param time A string representing the name of the column in `data` that contains time information.
#'             The column should be in "yyyy-mm-dd" format or any format recognizable by `as.Date()`.
#' @param group (Optional) A character vector representing the names of the columns in `data`
#'             that contain grouping information. Required if `data` contains multiple spatial units.
#' @return A character string indicating the predominant time interval of the dataset.
#'         Possible values: "day", "week", "month", "year", or "unknown".
#' @noRd


.get_time_interval <- function(data, time, group = NULL) {
  
  # Check if 'data' and 'time' are provided
  if (missing(data) || missing(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check if 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  # Check if 'time' column exists in 'data'
  if (!(time %in% names(data))) {
    stop(paste("Time column", time, "not found in 'data'."))
  }
  
  # Input validation for 'group'
  if (!is.null(group) && !all(names(group) %in% names(data))) {
    stop("Grouping variable names in 'group' should match column names in 'data'.") 
  }
  
  # Check for duplicated time entries
  if (!is.null(group)) {
    duplicated_rows <- duplicated(data[, c(group, time), drop = FALSE])
    if (any(duplicated_rows)) {
      stop("Duplicated time units detected. Ensure each group has unique time points.")
    }
  } else { 
    if (any(duplicated(data[[time]]))) { 
      stop("Duplicated time units detected. If the data contains multiple spatial units, specify them using the 'group' argument.")
    }
  }  
  
  # Select, sort and tranform time column
  if(!is.null(group)) {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(group, time)))) 
    date_vector <- as.Date(data[[time]])
  } else {
    data <- data |>
      dplyr::arrange(dplyr::across(all_of(c(time)))) 
    if(is.vector(data)) {
      date_vector <- as.Date(data)
    } else {
      date_vector <- as.Date(data[[time]])
    }
  }
  
  # Check for failed date conversion
  if (any(is.na(date_vector))) {
    stop("NA detected. Ensure 'time' column is in 'yyyy-mm-dd' format.")
  }
  
  # Calculate difference between the first two dates
  date_diffs <- diff(date_vector[1:2])
  # Determine the time interval based on the difference
  if (date_diffs == 1) {
    t <- "day"
  } else if (date_diffs == 7) {
    t <- "week"
  } else if (date_diffs >= 28 && date_diffs <= 31) {
    t <- "month"
  } else if (date_diffs >= 350 && date_diffs <= 370) {
    t <- "year"
  } else {
    t <- "unknown"
  }
  # Return the identified time interval as a string
  return(t)
}


# check_consecutive -----------------------------------------------------------
#' 
#' Check consecutive time points
#'  
#' Verifies whether time values in a dataset are consecutive based on the detected time interval
#' (e.g., daily, weekly, monthly, yearly). If a `group` column is specified, the check is performed within each group.
#'
#' @param data A data frame containing the dataset. Must include the time variable
#'             and optionally a group variable.
#' @param time A string representing the name of the column in `data` that contains 
#'             time values. The column should be in "yyyy-mm-dd" format or any 
#'             format recognizable by `as.Date()`.
#' @param group (Optional) A character vector representing the names of the columns
#'             in `data` that contain grouping information. If provided, the check will
#'             be performed separately within each group.
#'
#' @return A logical value: `TRUE` if all time values (within each group if `group` is specified)
#'         are consecutive, and `FALSE` otherwise.
#' @noRd

.check_consecutive <- function(data, time, group = NULL) {  
  
  # Check if 'data' and 'time' are provided
  if (missing(data) || missing(time)) {
    stop("'data' and 'time' must be provided.")
  }
  
  # Check if 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  # Check if 'time' column exists in 'data'
  if (!(time %in% names(data))) {
    stop(paste("Time column", time, "not found in 'data'."))
  }
  
  # Input validation for 'group'
  if (!is.null(group)) {
    if (!all(group %in% names(data))) {
      stop("All grouping variable names in 'group' should match column names in 'data'.") 
    }
  }
  
  # Check for duplicated time entries
  if (!is.null(group)) {
    duplicated_rows <- duplicated(data[, c(group, time), drop = FALSE])
    if (any(duplicated_rows)) {
      stop("Duplicated time units detected. Ensure each group has unique time points.")
    }
  } else { 
    if (any(duplicated(data[[time]]))) { 
      stop("Duplicated time units detected. If the data contains multiple spatial units, specify them using the 'group' argument.")
    }
  }  
  
  # Subset relevant columns (group and time)
  # Ensure `time` is in Date format
  data <- data |> 
    dplyr::mutate(!!time := as.Date(.data[[time]])) |>
    dplyr::select(all_of(c(group, time)))
  
  # Determine the time interval (e.g., day, week, month, year)
  interval <- .get_time_interval(data, time = time, group = group)
  
  # Check consecutiveness within each 'group' if group is provided 
  if (!is.null(group)) {
    is_consecutive <- data |>
      dplyr::arrange(dplyr::across(all_of(c(group, time)))) |>
      dplyr::group_by(dplyr::across(all_of(group))) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      ), .groups = "drop") |>
      dplyr::pull(.data$all_consecutive)
    
    # Combine results across all groups
    is_consecutive <- all(is_consecutive)
  } else {
    # Check consecutiveness when 'group' is not specified
    is_consecutive <- data |>
      dplyr::arrange(.data[[time]]) |>
      dplyr::mutate(date_diffs = as.numeric(difftime(.data[[time]], 
                                                     dplyr::lag(.data[[time]]), 
                                                     units = "days"))) |>
      dplyr::filter(!is.na(.data$date_diffs)) |>
      dplyr::summarize(all_consecutive = dplyr::case_when(
        interval == "day"   ~ all(.data$date_diffs == 1),
        interval == "week"  ~ all(.data$date_diffs == 7),
        interval == "month" ~ all(.data$date_diffs >= 28 & .data$date_diffs <= 31),
        interval == "year"  ~ all(.data$date_diffs >= 350 & .data$date_diffs <= 370),
        TRUE ~ FALSE
      )) |>
      dplyr::pull(.data$all_consecutive)
  }
  
  # return the consecutivness as TRUE / FALSE  
  return(is_consecutive)
}
