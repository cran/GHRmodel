#' Dengue cases from the "Mato Grosso do Sul" state of Brazil
#'
#' The `dengue_MS` example data set contains monthly counts of notified dengue cases by 
#' microregion, along with a range of spatial and spatiotemporal covariates 
#' (e.g., environmental, socio-economic and meteo-climatic factors). 
#' This data set represents a subset of a larger national data set that covers 
#' the entire territory of Brazil. The subset focuses on a specific region, 
#' *Mato Grosso do Sul*, for the purposes of illustration and computational efficiency.
#' See \code{@source} for access to the complete data set. 
#'
#' @format
#' A data frame with 2,600 rows and 27 columns:
#' \describe{
#'   \item{`micro_code`}{Unique ID number to each
#'   micro region (11 units)}
#'   \item{`micro_name`}{Name of each micro region}
#'   \item{`micro_name_ibge`}{Name of each micro region following IBGE}
#'   \item{`meso_code`}{Unique ID number  to each
#'   meso region (4 units)}
#'   \item{`meso_name`}{Name of each meso region}
#'   \item{`state_code`}{Unique ID number to each
#'   state (1 unit)}
#'   \item{`state_name`}{Name of each state}
#'   \item{`region_code`}{Unique ID number given to each Brazilian Region,
#'   In this data frame all observations come from the "Southeast Region"}
#'   \item{`region_name`}{Name of each Brazilian Region,
#'   In this data frame all observations come from the "Southeast Region"}
#'   \item{`biome_code`}{Biome code}
#'   \item{`biome_name`}{Biome name}
#'   \item{`ecozone_code`}{Ecozone code}
#'   \item{`ecozone_name`}{Ecozone name}
#'   \item{`main_climate`}{Most prevalent climate regime in the microregion.
#'   Based on Koppen Geiger climate regimes}
#'   \item{`month`}{Calendar month index, 1 = January, 12 = December}
#'   \item{`year`}{Year 2000 - 2019}
#'   \item{`time`}{Time index starting at 1 for January 2000}
#'   \item{`dengue_cases`}{Number of notified dengue cases registered in the
#'    notifiable diseases system in Brazil (SINAN) in the microregion of
#'    reference, at the month of first symptoms}
#'   \item{`population`}{Estimated population, based on projections calculated
#'     using the 2000 and 2010 censuses, and counts taken in 2007 and 2017}
#'   \item{`pop_density`}{Population density (number of people per km2)}
#'   \item{`tmax`}{Monthly average daily maximum temperature; gridded values
#'    (at a 0.5 deg resolution) averaged across each microregion}
#'   \item{`tmin`}{Monthly average daily minimum temperature; gridded values
#'    (at a 0.5 deg resolution) averaged across each microregion}
#'   \item{`pdsi`}{Self-calibrated Palmer drought severity index for each
#'   microregion. It measures how wet or dry a region is relative to usual
#'    conditions. Negative values represent periods of drought,
#'    positive values represent wetter periods. Calculated by taking the mean
#'    value within each microregion}
#'   \item{`urban`}{Percentage of inhabitants living in urban areas (2010 census)}
#'   \item{`water_network`}{Percentage of inhabitants with access to the piped
#'   water network according to the 2010 census}
#'   \item{`water_shortage`}{Frequency of reported water shortages per microregion
#'    between 2000 - 2016}
#'   \item{`date`}{First day of the Month, in date format ("%d-%m-%Y")}
#' }
#' @source \href{https://github.com/drrachellowe/hydromet_dengue}{source code on GitHub}; 
#'  \href{https://zenodo.org/records/4632205}{source code on Zenodo}; 

"dengue_MS"
