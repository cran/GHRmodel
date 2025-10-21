## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----install GHRmodel, eval=FALSE---------------------------------------------
# # Install from CRAN
# install.packages("GHRmodel")
# 
# # Get the development version from Gitlab
# library(devtools)
# devtools::install_git('https://earth.bsc.es/gitlab/ghr/ghrmodel.git')

## ----install R-INLA, eval=FALSE-----------------------------------------------
# install.packages("INLA",
#                  repos=c(getOption("repos"),
#                          INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

## ----example long data, echo=FALSE--------------------------------------------

library(GHRmodel)
library(dplyr)
data("dengue_MS")

example_df<- dengue_MS |>  
  # Filter out the year 2000 
  filter(year > 2000 & year < 2005) |>  
  filter(micro_name == "Alto Taquari" | micro_name == "Alto Taquari") |> 
  select(c("date", "micro_name","dengue_cases", "tmin")) |> 
  rename(region_name = micro_name)

head(example_df)


## ----GHRmodel_structure,fig.width=10, fig.height=10, fig.align='center', out.width='100%', echo= FALSE, fig.cap="Structure of the **GHRmodel** package, outlining its functions (in blue), GHRmodel-specific output objects (in purple), generic output objects (in grey), and general functionality. Generic output objects can be provided directly by the user or can be generated using GHRmodel helper functions."----

knitr::include_graphics("https://gitlab.earth.bsc.es/ghr/ghrmodel/-/raw/510e4737684359badce248d83a9113be8a3b976f/inst/images/GHRmodel_structure.png")

## ----cov function table, echo = FALSE-----------------------------------------
knitr::kable(
  data.frame(
    Function = c("`extract_names()`", "`cov_uni()`", "`cov_nl()`", "`cov_interact()`", "`cov_varying()`", "`cov_multi()`","`cov_add()`"),
    Purpose = c(
      "Selects covariate names from a dataset",
      "Prepares covariates for univariable INLA models",
      "Converts covariates to non-linear effect terms, with optional replication.",
      "Creates interaction terms between 2 or 3 covariates (e.g., var1:var2).",
      "Creates spatially or temporally varying effect terms.",
      "Generates combinations of covariates for multivariable models.",
      "Adds a covariate to each element of a covariate list."
    )),
  format = "markdown",
  caption = "Overview of helper functions to prepare covariate lists for INLA model formulas."
)


## ----Load required libraries, eval=FALSE--------------------------------------
# # Load necessary package dependencies
# library(dplyr)        # Data manipulation
# library(tidyr)        # Data tidying
# library(tidyselect)   # Helpers for selecting variables programmatically
# library(rlang)        # Tools for tidy evaluation and non-standard evaluation in tidyverse code
# library(ggplot2)      # Data visualization: creating plots and graphs
# library(cowplot)      # Combining and arranging multiple ggplot2 plots into a single figure
# library(grDevices)    # Base R graphics device functions (e.g., color palettes, saving plots)
# library(RColorBrewer) # Predefined color palettes for plots
# library(colorspace)   # Advanced color space manipulation and palettes
# library(sf)           # Handling spatial vector data (simple features)
# library(spdep)        # Spatial dependence and autocorrelation analysis
# library(sn)           # Skew-normal and skew-t distributions (for modeling skewed data)
# library(INLA)         # Integrated Nested Laplace Approximation for Bayesian models
# library(GHRexplore)   # Exploratory analysis of health data
# 
# # Load GHRmodel
# library(GHRmodel)

## ----Load required libraries invisible, echo=FALSE----------------------------
# Load necessary package dependencies
library(dplyr)        # Data manipulation
library(tidyr)        # Data tidying
library(tidyselect)   # Helpers for selecting variables programmatically
library(rlang)        # Tools for tidy evaluation and non-standard evaluation in tidyverse code
library(ggplot2)      # Data visualization: creating plots and graphs
library(cowplot)      # Combining and arranging multiple ggplot2 plots into a single figure
library(grDevices)    # Base R graphics device functions (e.g., color palettes, saving plots)
library(RColorBrewer) # Predefined color palettes for plots
library(colorspace)   # Advanced color space manipulation and palettes
library(sf)           # Handling spatial vector data (simple features)
library(spdep)        # Spatial dependence and autocorrelation analysis
library(sn)           # Skew-normal and skew-t distributions (for modeling skewed data)
library(GHRexplore)   # Exploratory analysis of health data

# Load GHRmodel
library(GHRmodel)     

## ----Load dengue_MS-----------------------------------------------------------
#Load data 
data("dengue_MS")
df <- dengue_MS

# View initial rows in example dataframe
head(df)


## ----Process the data set-----------------------------------------------------
# Check that the date variable is in Date format
class(df$date)

# Create ID variables
df <- df |>  
  # Filter out the year 2000. There are no dengue cases for that year. 
  filter(year > 2000) |>  
  # Create numeric IDs for year, month and various spatial units.
  mutate(
    year_id = as.numeric(as.factor(year)),
    month_id = as.numeric(as.factor(month)),
    spat_id = as.numeric(as.factor(micro_code)), 
    spat_meso_id = as.numeric(as.factor(meso_code))
  )


## ----Load map_MS, fig.width=5, fig.height=5-----------------------------------
# Load map (sf object) included in package
data("map_MS")

# Visualize the map with microregion labels
ggplot() +
  geom_sf(data = map_MS) +
  geom_sf_text(data = map_MS, aes(label = code), size = 2.5) +
  theme_minimal() +
  labs(title = "Mato Grosso do Sul",
       x = "Longitude",
       y = "Latitude")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## ----Adjacencies Matrix-------------------------------------------------------
# Create adjacency Matrix
nb <- spdep::poly2nb(map_MS)
g <- spdep::nb2mat(nb, style = "B")

## ----lag_cov lags 1 to 6------------------------------------------------------
data <- lag_cov(data = df,
                name = c("tmin", "pdsi"),  # variables to lag 
                time = "date",        # time variable 
                lag = c(1:6),         # 1 to 6-month lags
                group = "micro_code", # identify spatial units with independent time series
                add = TRUE)           # lagged variables appended to original data

# Visualize lagged variables
head(data[32:43])


## ----Extracts variable names--------------------------------------------------

# Extract variable names matching the specified patterns
cov_names <- extract_names(data = data,
                           pattern = c("tmin.",
                                       "pdsi."))

# Visualize output: character vector of covariate names
glimpse(cov_names)

## ----univariable models-------------------------------------------------------

# Generate list of single linear covariate names
uni_cov_lin <- cov_uni(covariates = cov_names, # Input character vector of covariate names
                       pattern = c("pdsi.l",  # Select lagged pdsi and tmin from the vector of covariate names
                                   "tmin.l"))

# Visualize output: list of single linear covariate names
head(uni_cov_lin,2)

## ----Non linear univariable---------------------------------------------------

# Generate list of single non-linear covariate names
uni_cov_nl <- cov_nl(covariates = cov_names, 
                     method = "quantile",
                     model = "rw2",
                     pattern = c("pdsi", "tmin"), 
                     n = 10,
                     add =FALSE)

# Visualize output: list of single non-linear covariate names
head(uni_cov_nl,2)

## ----Combine linear and non-linear covariates---------------------------------
# Generate list of combinations of linear covariate names
multi_cov_lin <- cov_multi(covariates = uni_cov_lin,
                           pattern = c("pdsi","tmin"),
                           add = FALSE)

# Visualize output: list of combinations of linear covariate names
head(multi_cov_lin,2)

# Generate list of combinations of non-linear covariate names
multi_cov_nl <- cov_multi(covariates = uni_cov_nl,
                          pattern = c("pdsi","tmin"),
                          add = FALSE)

# Visualize output: list of combinations of non-linear covariate names
head(multi_cov_nl,2)


## ----Define monthly and yearly random effects priors--------------------------
# Define Gamma priors for the precision of temporal random effects
prior_t <- list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))) 

## ----Define spatial random effect priors--------------------------------------
# Define penalized complexity (PC) priors for spatial random effects using BYM2
prior_sp <- list(
  prec = list(prior = 'pc.prec', param = c(0.5 / 0.31, 0.01)),  # Precision of spatial effect
  phi  = list(prior = 'pc', param = c(0.5, 2 / 3))              # Mixing parameter: structured vs unstructured
)


## ----List of covariate combinations-------------------------------------------

cov_list <- c(list(uni_cov_lin[[7]],    # Include linear tmin.l1 
                   uni_cov_nl[[1]],     # Include non-linear tmin.l1
                   multi_cov_lin[[1]],  # Include linear "pdsi.l1" "tmin.l1"
                   multi_cov_nl[[1]]),  # Include non-linear "pdsi.l1" "tmin.l1" combination
              uni_cov_nl[7:12])         # Include non-linear terms for pdsi.l1 through pdsi.l6

# Visualize the first 3 elements of the covariate list
head(cov_list, 4)

# The result is a list
class(cov_list)


## ----formulas_cov_list--------------------------------------------------------

# Build a list of INLA-compatible model formulas including outcome, covariates, and random effects
formulas_cov_list <- write_inla_formulas(
  # Outcome (dependent variable) in all formulas
  outcome = "dengue_cases",
  # List of covariate sets to be included in the formulas (generated earlier as cov_list)
  covariates = cov_list,
  # First random effect: monthly seasonality
  re1 = list(
    id        = "month_id",      # Time index for monthly effect
    model     = "rw1",           # First-order random walk model
    cyclic    = TRUE,            # Enforce cyclicity over months (Dec → Jan wrap-around)
    hyper     = "prior_t",       # Hyperprior object for precision
    replicate = "spat_meso_id"   # Separate seasonal pattern per mesoregion
  ),
  # Second random effect: inter-annual trend
  re2 = list(
    id    = "year_id",           # Year index
    model = "iid",               # Independent and identically distributed
    hyper = "prior_t"            # Hyperprior for temporal smoothness
  ),
  # Third random effect: spatial autocorrelation
  re3 = list(
    id    = "spat_id",           # Spatial unit identifier
    model = "bym2",              # BYM2 spatial model (structured + unstructured)
    graph = "g",                 # Pre-computed spatial adjacency graph
    hyper = "prior_sp"           # PC priors for BYM2 model parameters
  ),
  # Include a baseline random effect-only model formula as 1st element in the list
  baseline = TRUE
)

# Example of INLAcompatible model formula generated
formulas_cov_list[2]

# Outputs a character vector of model formulas
class(formulas_cov_list)

## ----formulas_cov_list_ghr----------------------------------------------------
formulas_cov_list_ghr <- as_GHRformulas(formulas = formulas_cov_list)

# Outputs a GHRformulas object
class(formulas_cov_list_ghr)

## ----fit_models model_list, eval = FALSE--------------------------------------
# 
# model_list <- fit_models(
#   formulas = formulas_cov_list_ghr, # GHRformulas object
#   data = data,                      # Data to fit the models
#   family = "nbinomial",             # Negative binomial likelihood
#   name = "mod",                     # Label prefix for each model
#   offset = "population",            # Offset variable to account for population size
#   control_compute = list(
#     config = FALSE,                 # Extract from INLA summary.fitted.values
#     vcov = FALSE                    # Do not return variance-covariance matrix
#   ),
#   nthreads = 8                      # Use 8 threads for parallel computation
# )
# 
# # Outputs a GHRmodels object
# class(model_list)
# 
# # Extract goodness of fit metrics as a data.frame
# model_list_gof <- model_list$mod_gof
# 

## ----fit_models model_list readrds, echo = FALSE------------------------------
model_list<- readRDS(system.file("examples", "model_list.rds", package = "GHRmodel"))

# Extract goodness of fit metrics as a data.frame
model_list_gof <- model_list$mod_gof 

## ----rank_models--------------------------------------------------------------

best_waic <- rank_models(
  models = model_list,  # GHRmodels object containing model fit results
  metric = "waic",      # Metric used to rank models (lower WAIC is better)
  n = 2                 # Number of top-ranked models to return
)

# Display the model IDs of the two best models
best_waic


## ----sample_ppd, eval=FALSE---------------------------------------------------
# ppd_df <- sample_ppd(
#   model_list,
#   mod_id = "mod3",
#   s = 100, # Increase to have more robust results
#   nthreads = 2)

## ----ppd_df readrds, echo = FALSE---------------------------------------------
ppd_df <- readRDS(
  system.file("examples", "ppd_df.rds", package = "GHRmodel"))

## ----plot_ppd, fig.width=6, fig.height=3--------------------------------------
plot_ppd(ppd_df)

## ----model_list_mod_gof-------------------------------------------------------
# Annotate each model in the model_list$mod_gof dataframe with descriptive labels
model_list_gof <- model_list$mod_gof |> 
  dplyr::mutate(
    `Variable transformation 1st covariate` = dplyr::case_when(
      grepl("_nl", covariate_1, ignore.case = TRUE) ~ "non-linear", 
      is.na(covariate_1) ~ "baseline",
      T ~ "linear"
    ),
    `Covariates` = dplyr::case_when(
      grepl("tmin", covariate_1, ignore.case = TRUE) & is.na(covariate_2) ~ "tmin",
      grepl("pdsi", covariate_1, ignore.case = TRUE) & is.na(covariate_2) ~ "pdsi",
      !is.na(covariate_1) & !is.na(covariate_2) ~ "tmin + pdsi",
      is.na(covariate_1) ~ "random effect only"
    )
  )

## ----plot_gof covariate 1 and 2, fig.width=7, fig.height=4--------------------
# Plot change in WAIC for each model compared to the baseline (first model in the list)
plot_gof(
  mod_gof = model_list_gof,            # Goodness-of-fit data from model_list
  metric = "waic_vs_first",            # Compare WAIC to the first model (baseline)
  ci = TRUE,                           # Include credible intervals (if available)
  var_arrange = "waic_vs_first",       # Arrange models by WAIC difference
  var_color =  "Variable transformation 1st covariate",    # variable transformation
  var_shape = "Covariates",  # Shape mapped to including tmin.l1
  palette = "Colorblind")    

## ----plot_fit default, fig.width=7, fig.height=4------------------------------
# Plot observed vs. fitted cases over time for three selected models
plot_fit(
  models = model_list,                         # A GHRmodels object (fitted models)
  mod_id = c("mod1", "mod3", "mod5"),          # Vector of model IDs to plot
  mod_label = c("Baseline",                    # Custom display names 
                "tmin.l1.nl",            
                "pdsi.l1.nl_tmin.l1.nl"),            
  ref_color = "grey",                          # Color override for the ref model (first)
  time = "date",                               # Name of the time variable in the dataset
  palette = "Set2",                            # Color palette for fitted lines
  xlim = c("2010-01-01", "2020-01-01"),        # Limit x-axis to this date range
  title = "Fitted vs Observed"                 # Main plot title
)


## ----plot_fit facet by group, fig.width=9, fig.height=5, out.width='100%'-----
# Plot observed vs. fitted values from multiple models with faceted panels by model
plot_fit(
  model = model_list,                          # GHRmodels object containing fitted models
  mod_id = c("mod1", "mod4"),                  # Vector of model IDs to plot
  mod_label = c("Baseline",                    # Custom display name for mod1
                "pdsi.l1_tmin.l1"),            # Custom display name for mod4
  time = "date",                               # Name of the time variable
  group = "meso_name",                         # Column defining grouped time-series (meso region)
  group_id = c("Pantanais Sul Mato-Grossense", # Select specific groups to display
               "Leste De Mato Grosso Do Sul"),
  transform = "log10p1",                       # Apply log10(x + 1) transformation to y-axis for scaling
  palette = "Set2",                            # Color palette for model fits
  mod_facet = TRUE,                            # Facet by model to compare fits across panels
  obs_color = "purple",                        # Line color for observed case counts
  obs_label = "Observed cases",                # Legend label for observed values
  ref_color = "grey",                          # Override color for the reference model
  title = "Fitted models vs. Observed Cases",  # Main plot title
  ci = FALSE                                   # Do not plot credible intervals
)

## ----pot_coef_lin, fig.width = 6, fig.height = 4------------------------------
plot_coef_lin(
  model = model_list,
  mod_id = c("mod2","mod4"),
  var_label = c("tmin.l1"= "Min. temp lag 1",
                "pdsi.l1" = "Drought index lag 1"),
  title = "Effects of linear covariates"
)

## ----plot_coef_nl grid 6 variables,  fig.width = 6, fig.height = 10-----------
plot_coef_nl(
  # List of fitted models from which to extract nonlinear effects
  models = model_list,
  # Model IDs to include in the plot (must match names in `model_list`)
  mod_id = c(paste0("mod", 6:11)),
  # Custom labels for the selected models (used for legend or facet titles)
  mod_label = c(
    "mod6"  = "pdsi.l1_nl",
    "mod7" = "pdsi.l2_nl", 
    "mod8" = "pdsi.l3_nl",
    "mod9" = "pdsi.l4_nl",
    "mod10" = "pdsi.l5_nl", 
    "mod11" = "pdsi.l6_nl"
  ),
  # Custom labels for the nonlinear covariates (used in axis or strip labels)
  var_label = c(
    "pdsi.l1" = "Drought index (PDSI) lag 1",
    "pdsi.l2" = "Drought index (PDSI) lag 2",
    "pdsi.l3" = "Drought index (PDSI) lag 3",
    "pdsi.l4" = "Drought index (PDSI) lag 4",
    "pdsi.l5" = "Drought index (PDSI) lag 5", 
    "pdsi.l6" = "Drought index (PDSI) lag 6"
  ),
  # Include only variables whose names contain "pdsi"
  pattern = "pdsi",
  # Title for the entire plot
  title = "Change in non-linear term for drought index (PDSI) with and without mean min. temp lag 1",
  # Label for the x-axis
  xlab = "PDSI",
  # Color palette to use for plotting (a predefined IDExtremes palette)
  palette = "IDE2",
  # Grid display mode
  collapse = FALSE,
  # Disable rug plot on the x-axis
  rug = FALSE
)

## ----plot_coef_nl collapse 2 variables, fig.width = 6, fig.height = 4---------
plot_coef_nl(
  models = model_list,
  mod_id = c( "mod5", "mod6") ,
  mod_label = c("mod6" = "pdsi.l1_nl",
                "mod5" = "pdsi.l1_nl + tmin.l1_nl"),
  var_label = c("pdsi.l1" = "Drought index (PDSI)"),
  name = c("pdsi.l1"),
  title = "Change in non-linear term for drought index (PDSI) \n with and without mean min. temp lag 1",
  xlab = "PDSI",
  palette = "IDE2",
  collapse = TRUE         # Combine all plots into a single panel
)



## ----plot_re yearly no replication,  fig.width = 7, fig.height = 4------------
plot_re(
  model = model_list, # A GHRmodels object containing fitted model outputs
  mod_id = c("mod1", "mod3", "mod5"), # IDs of the models to compare (must match those in model_list)
  
  mod_label = c("Baseline", # Label for mod1: baseline model
                "tmin.l1_nl", # Label for mod4: includes PDSI replicated by climate
                "pdsi.l1_nl + tmin.l1_nl"), # Label for mod6: includes nonlinear effects of PDSI and Tmin
  
  re_id = "year_id",  # Name of the random effect variable (as defined in model structure)
  re_label = "year",  # Optional: label to map year_id to calendar years
  
  ref_color = "grey",  # Color to use for the first (reference) model’s effects
  palette = "IDE2", # Color palette for other model effects
  
  title = "Yearly Random Effect", # Title for the plot
  xlab = "Year" # Label for the x-axis (since re_label maps ID to actual years)
)

## ----plot_re custom data------------------------------------------------------
# Maps integers 1–12 to "January" through "December" using R’s built-in month.name vector
model_list[["data"]] <- model_list[["data"]]|>
  dplyr::mutate(month_label = month.name[month_id]) 

## ----plot_re monthly replication, fig.width = 7, fig.height = 6---------------
plot_re(
  models = model_list, # A GHRmodels object containing fitted model outputs
  
  mod_id = c("mod1", "mod3", "mod5"), # IDs of the models to compare (must match those in model_list)
  
  mod_label = c("Baseline", # Label for mod1: baseline model
                "tmin.l1_nl", # Label for mod4: includes PDSI replicated by climate
                "pdsi.l1_nl + tmin.l1_nl"), # Label for mod6: includes nonlinear effects of PDSI and Tmin
  
  re_id = "month_id", # Name of the random effect in the model formula (e.g., f(month_id, ...))
  re_label = "month_label", # Variable in the dataset corresponding to readable month names
  
  rep_id = "spat_meso_id", # Variable indicating the grouping structure (replication across meso areas)
  rep_label = "meso_name",# Variable to label facets (e.g., "Centro Norte", "Sudoeste", etc.)
  
  ref_color = "grey", # Color for the reference model (first listed in mod_id)
  palette = "IDE2", # Palette for coloring additional models
  
  title = "Monthly random effect repeated by meso area",  # Plot title
  xlab = "Month" # Label for x-axis (calendar month, from re_label)
)

## ----plot_re spatial no map, fig.width = 7, fig.height = 4--------------------
plot_re(
  models = model_list, # A GHRmodels object containing the output of fitted models
  
  mod_id = c("mod1", "mod3", "mod5"), # IDs of the models to compare (must match those in model_list)
  
  mod_label = c("Baseline", # Label for mod1: baseline model
                "tmin.l1_nl", # Label for mod4: includes PDSI replicated by climate
                "pdsi.l1_nl + tmin.l1_nl"), # Label for mod6: includes nonlinear effects of PDSI and Tmin
  
  re_id = "spat_id",  # Name of the spatial random effect variable as defined in the model
  re_label = "micro_name", # Optional label to map spat_id to readable microregion names 
  
  title = "Spatial random effects",  # Title of the plot
  xlab = "Micro code for spatial units" # Label for the x-axis 
)

## ----plot_re spatial map, fig.width = 7, fig.height = 3-----------------------
plot_re(
  models = model_list, # A GHRmodels object containing the output of fitted models
  
  mod_id = c("mod1", "mod3", "mod5"), # IDs of the models to compare (must match those in model_list)
  
  mod_label = c("Baseline", # Label for mod1: baseline model
                "tmin.l1_nl", # Label for mod4: includes PDSI replicated by climate
                "pdsi.l1_nl + tmin.l1_nl"), # Label for mod6: includes nonlinear effects of PDSI and Tmin
  
  re_id = "spat_id", # Name of the spatial random effect variable used in model
  re_label = "micro_name", # Variable used to map IDs to readable names
  
  map = map_MS, # An sf object with polygon boundaries for plotting
  map_area = "code", # Name of the column in map_MS that matches spat_id in the model
  
  title = "Spatial random effects", # Title for the spatial plot
  
  legend = "Median of the \nrandom effect ppd" # Title for the legend
)

## ----subset-model-example-----------------------------------------------------
# Extract a vector with the model IDs of the 2 best fitting models by WAIC
best_waic <- rank_models(
  models = model_list,  # GHRmodels object containing model fit results
  metric = "waic",      # Metric used to rank models (lower WAIC is better)
  n = 2                 # Number of top-ranked models to return
)

# The output is a vector 
best_waic

# Subset those specific models and assign new IDs
model_waic <- subset_models(
  model = model_list,
  mod_id = best_waic,
  new_name = "best_waic"
)

# The output is a reduced GHRmodels object
class(model_waic)

# Inspect the model IDs of the subset GHRmodels object
head(model_waic$mod_gof$model_id) 

## ----get-covariates-example---------------------------------------------------
# Get all covariate combinations as declared in the best-fitting models
best_waic_cov <- get_covariates(model = model_waic, unique = FALSE)

# Visualize covariate combinations 
best_waic_cov

# Get all unique covariates across the best-fitting models
best_waic_cov_unique <- get_covariates(model = model_waic, unique = TRUE)

# Visualize unique covariates 
best_waic_cov_unique

## ----Add urban to a list of covariates----------------------------------------
best_waic_cov_urban <- cov_add(best_waic_cov, name= "urban")

head(best_waic_cov_urban)

## ----formulas_cov_list_urban--------------------------------------------------

# Build a list of INLA-compatible model formulas including outcome, covariates, and random effects
formulas_cov_urban_list <- write_inla_formulas(
  # Outcome (dependent variable) in all formulas
  outcome = "dengue_cases",
  # List of covariate sets to be included in the formulas (generated earlier as cov_list)
  covariates = best_waic_cov_urban ,
  # First random effect: monthly seasonality
  re1 = list(
    id        = "month_id",      # Time index for monthly effect
    model     = "rw1",           # First-order random walk model
    cyclic    = TRUE,            # Enforce cyclicity over months (Dec → Jan wrap-around)
    hyper     = "prior_t",       # Hyperprior object for precision
    replicate = "spat_meso_id"   # Separate seasonal pattern per mesoregion
  ),
  # Second random effect: inter-annual trend
  re2 = list(
    id    = "year_id",           # Year index
    model = "rw1",               # First-order random walk model
    hyper = "prior_t"            # Hyperprior for temporal smoothness
  ),
  # Third random effect: spatial autocorrelation
  re3 = list(
    id    = "spat_id",           # Spatial unit identifier
    model = "bym2",              # BYM2 spatial model (structured + unstructured)
    graph = "g",                 # Pre-computed spatial adjacency graph
    hyper = "prior_sp"           # PC priors for BYM2 model parameters
  ),
  # Include a baseline random effect-only model formula as 1st element in the list
  baseline = TRUE
)

# Convert into a GHRformulas object 
formulas_cov_urban_list_ghr <- as_GHRformulas(formulas = formulas_cov_urban_list)

## ----fit_models model_urban_list, eval = FALSE--------------------------------
# model_urban_list <- fit_models(
#   formulas = formulas_cov_urban_list_ghr, # GHRformulas object
#   data = data,                      # Data to fit the models
#   family = "nbinomial",             # Negative binomial likelihood
#   name = "mod",                     # Label prefix for each model
#   offset = "population",            # Offset variable to account for population size
#   control_compute = list(
#     config = FALSE,                 # Do not posterior predictive distribution
#     vcov = FALSE                    # Do not return variance-covariance matrix
#   ),
#   nthreads = 8                      # Use 8 threads for parallel computation
# )

## ----fit_models model_urban_list readrds, echo = FALSE------------------------
model_urban_list <- readRDS(system.file("examples", "model_urban_list.rds",
                                       package = "GHRmodel"))

## ----stack-model-example------------------------------------------------------
# Merge models from different objects
model_stack <- stack_models(
  model_waic,
  model_urban_list,
  new_name = "mod"
)

# The combined model_stack combines the best fitting models by WAIC and those including the urban covariate
model_stack$mod_gof$model_id  

## ----rank_models model_stack--------------------------------------------------
rank_models(model_stack, 
            metric = "waic")

## ----model_stack_mod_gof------------------------------------------------------
# Add a variable to the mod_gof dataframe to distinguish models with the urban covariate
model_stack_gof  <- model_stack$mod_gof |> 
  dplyr::mutate(
    `Add urban` = dplyr::case_when(
      grepl("urban", covariate_3, ignore.case = TRUE) |
        grepl("urban", covariate_2, ignore.case = TRUE)
      ~ "yes", 
      is.na(covariate_3) == TRUE ~ "no"
    ))

## ----plot_gof model_stack, fig.width=6, fig.height=4--------------------------
# Plot change in WAIC for each model compared to the baseline (first model in the list)
plot_gof(
  mod_gof = model_stack_gof,           # Goodness-of-fit data from model_list
  metric = "waic",                     # Compare WAIC 
  ci = TRUE,                           # Include credible intervals (if available)
  var_arrange = "waic",                # Arrange models by WAIC 
  var_color =  "Add urban", 
  mod_label = c(
    "mod1" = "Baseline",                 
    "mod2" = "tmin.l1_nl + urban",      
    "mod3" = "tmin.l1_nl + pdsi.l1_nl + urban", 
    "best_waic1" = "tmin.l1_nl", 
    "best_waic2" = "tmin.l1_nl + pdsi.l1_nl"
  )
)

## ----pot_coef_lin model stack, fig.width=6, fig.height=4----------------------
plot_coef_lin(
  model = model_stack,
  title = "Effects of linear covariates"
)

