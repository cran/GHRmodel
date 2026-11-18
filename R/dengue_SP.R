#' Dengue cases from the "São Paulo" state of Brazil
#'
#' The `dengue_SP` example data set reports the weekly number of notified dengue
#' cases in the municipality of São Paulo together with climatic covariates.
#' Data was sourced from Infodengue (see source below).
#'
#' @format
#' A data frame with 624 rows and 9 columns:
#' \describe{
#'   \item{`date`}{First day of the week, in date format ("%Y-%m-%d")}
#'   \item{`geocode`}{IBGE municipality code for the city of São Paulo}
#'   \item{`cases`}{Number of notified dengue cases}
#'   \item{`year`}{Year 2010 - 2021}
#'   \item{`temp_med`}{Weekly average daily mean temperature}
#'   \item{`precip_tot`}{Weekly cumulative precipitation}
#'   \item{`enso`}{El Niño-Southern Oscillation Index}
#'   \item{`pop`}{Number of inhabitants}
#'   \item{`pop100000`}{Population divided by 100000.}
#' }
#'
#' @source \href{https://info.dengue.mat.br/services/api}{Infodengue API}

"dengue_SP"
