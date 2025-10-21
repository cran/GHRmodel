#' Dengue cases from the "São Paulo" state of Brazil
#'
#' The `dengue_SP` example data set reports the weekly number of notified dengue cases in
#' the municipality of São Paulo together with climatic covariates. Data was sourced from 
#' Infodengue (see \code{@source}). 
#'
#' @format 
#' A data frame with 678 rows and 8 columns:
#' \describe{
#'   \item{`date`}{First day of the week, in date format ("%d-%m-%Y")}
#'   \item{`geocode`}{Unique ID code for São Paulo microregion}
#'   \item{`cases`}{Number of notified dengue cases}
#'   \item{`year`}{Year 2000 - 2022}
#'   \item{`temp_med`}{Weekly average daily mean temperature}
#'   \item{`precip_tot`}{Weekly cumulative precipitation}
#'   \item{`enso`}{El Niño-Southern Oscillation Index}
#'   \item{`pop`}{Number of inhabitants}
#' }
#' 
#' @source \href{https://info.dengue.mat.br/services/api}{Infodengue API}

"dengue_SP"