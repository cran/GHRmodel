## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----GHRmodel_structure,fig.width=10, fig.height=10, fig.align='center', out.width='100%', echo= FALSE, fig.cap="**GHRmodel** functions to streamline INLA-compatible model formula development."----
knitr::include_graphics("https://gitlab.earth.bsc.es/ghr/ghrmodel/-/raw/510e4737684359badce248d83a9113be8a3b976f/inst/images/GHRmodel_structure.png")

## ----cov function table, echo = FALSE-----------------------------------------
knitr::kable(
  data.frame(
    Function = c("`extract_names()`", "`cov_uni()`", "`cov_multi()`", "`cov_nl()`",  "`cov_interact()`", "`cov_varying()`"),
    Purpose = c(
      "Selects covariate names from a dataset",
      "Prepares covariates for univariable INLA models",
      "Generates combinations of covariates for multivariable models.",
      "Convert covariates to nonlinear effect terms, with optional replication.",
      "Creates interaction terms between 2 or 3 covariates (e.g., var1:var2).",
      "Creates spatially or temporally varying effect terms using INLA's f() structure."
    ),
    `Input` = c(
      "Data frame",
      "Character vector",
      "Character vector or a list of character vectors",
      "Character vector or a list of character vectors",
      "List of character vectors",
      "Character vector or a list of character vectors"
    ),
    `Output` = c(
      "Character vector",
      "List of linear covariates",
      "List with INLA nonlinear effect terms",
      "List of multivariable covariate sets",
      "List including interaction terms",
      "List including spatially/temporally varying terms"
    )
  ),
  format = "markdown",
  caption = "Overview of helper functions to prepare covariate lists for INLA-compatible formulas."
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

## ----Process the data set-----------------------------------------------------
#Load data 
data("dengue_MS")
df <- dengue_MS

# Create ID variables
df <- df |>  
  # Filter out the year 2000 
  filter(year > 2000) |>  
  # Create numeric IDs for year, month and various spatial units.
  mutate(
    year_id = as.numeric(as.factor(year)),                # year numeric ID
    year_id2 = as.numeric(as.factor(year)),               # year second numeric ID
    month_id = as.numeric(as.factor(month)),              # month numeric ID
    spat_id = as.numeric(as.factor(micro_code)),          # microregion numeric ID
    spat_meso_id = as.numeric(as.factor(meso_code)),      # meso-region numeric ID
    main_climate_f = as.numeric(as.factor(main_climate))  # climate zone numeric ID
  )


## ----Load map_MS--------------------------------------------------------------
# Load map included in package
data("map_MS")

# Create adjacency Matrix
nb <- spdep::poly2nb(map_MS)
g <- spdep::nb2mat(nb, style = "B")

## ----lag_cov lags 1 to 6------------------------------------------------------
data <- lag_cov(data = df,
                name = c("tmin", "pdsi"),  # variables to lag 
                time = "date",             # time variable 
                lag = c(1:6),              # 1 to 6-month lags
                group = "micro_code",      # identify spatial units with independent time series
                add = TRUE)                # lagged variables appended to original data

# Visualize lagged variables
head(data[34:39])


## ----Define monthly and yearly random effects priors--------------------------
# Define Gamma priors for the precision of temporal random effects
prior_t <- list(prec = list(prior = 'loggamma', param = c(0.01, 0.01))) 

## ----Define spatial random effect priors--------------------------------------
# Define penalized complexity (PC) priors for spatial random effects using BYM2
prior_sp <- list(
  prec = list(prior = 'pc.prec', param = c(0.5 / 0.31, 0.01)),  # Precision of spatial effect
  phi  = list(prior = 'pc', param = c(0.5, 2 / 3))              # Mixing parameter: structured vs unstructured
)


## ----Extracts variable names--------------------------------------------------
# Extract variable names matching the specified patterns
cov_names <- extract_names(data = data,
                           pattern = c("tmin.",
                                       "pdsi.", 
                                       "urban",
                                       "main_climate_f"))
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

# Generate list of single nonlinear covariate names
uni_cov_nl <- cov_nl(covariates = cov_names, 
                     method = "quantile",
                     model = "rw2",
                     pattern = c("pdsi", "tmin"), 
                     n = 10,
                     add =FALSE)

# Visualize output: list of single nonlinear covariate names
head(uni_cov_nl,2)

## ----Replicated non linear univariable----------------------------------------
# Generate list of replicated nonlinear covariate names
uni_cov_nl_rep <- cov_nl(covariates = uni_cov_lin, 
                          method = "quantile",
                          pattern = c("pdsi"),
                          n = 10, 
                          replicate = "main_climate_f",
                          add = TRUE)

# Visualize output: list of replicated nonlinear covariate names
head(uni_cov_nl_rep[13:14])

## ----Combine linear and nonlinear replicated covariates-----------------------
# Create a list of combined predictors, some non linear and replicated
multi_cov_nl_rep <- cov_multi(covariates = uni_cov_nl_rep, 
                              pattern =  c("pdsi","tmin"))

# Visualize output: list of replicated nonlinear pdsi lagged terms combined with linear tmin lagged terms
head(multi_cov_nl_rep[7:8])

## ----multi_cov_lin------------------------------------------------------------
# Generate list of combinations of linear covariate names
multi_cov_lin <- cov_multi(covariates = uni_cov_lin,
                           pattern = c("pdsi","tmin"),
                           add = FALSE)

# Visualize output: list of combinations of linear covariate names
head(multi_cov_lin,2)

## ----triple_cov_lin-----------------------------------------------------------
# Add urban to each element of the bivariate covariate list 
triple_cov_lin <- cov_add(covariates = multi_cov_lin,
                          name = "urban")

# Visualize output: list of combinations of 3 linear covariate names
head(triple_cov_lin,2)

## ----interacting_cov----------------------------------------------------------
# Create a list of interacting linear predictors
interacting_cov <- cov_interact(covariates = triple_cov_lin,
                                pattern = c("pdsi","tmin", "urban"))

# Visualize output: list of interactions between linear pdsi terms and linear tmin terms
head(interacting_cov, 2)

## ----Varying covariates-------------------------------------------------------
# Create a list of varying univariable predictors
varying_cov <- cov_varying(covariates = multi_cov_lin,
                            pattern = c("pdsi"),
                            unit = "main_climate_f")

# Visualize output: list of combinations of lagged pdsi varying by main_climate_f and linear tmin terms
head(varying_cov,2)

## ----Varying vs. Replicated Effects in INLA table, echo = FALSE---------------
knitr::kable(
  data.frame(
    Feature = c(
      "Applied to",
      "Purpose",
      "INLA syntax",
      "Structure assumption",
      "Example use"
    ),
    `Varying Effect` = c(
      "Linear terms",
      "Group-specific linear slopes",
      "`f(group, covariate, model = 'iid')`",
      "Unstructured (`iid`)",
      "Region-specific slopes of the linear effect of rainfall"
    ),
    `Replicate Effect` = c(
      "Nonlinear terms",
      "Group-specific nonlinear functions",
      "`f(covariate, model = ..., replicate = group)`",
      "Same structure (e.g., `rw2`), different curves",
      "Region-specific nonlinear effect of rainfall"
    )
    ,
    check.names = FALSE
  ),
  caption = "Comparison of Replicate and Varying Effects in INLA"
)


## ----List of covariate combinations-------------------------------------------
# Build a combined list of various transformations and combinations of pdsi.l1 
cov_list <- c(
  list(
    uni_cov_lin[[1]],             # [1] pdsi.l1 (linear)
    uni_cov_nl_rep[[13]],         # [2] pdsi.l1 (nonlinear replicated by 'main_climate_f')
    multi_cov_lin[[1]],           # [3] pdsi.l1 (linear) + tmin.l1 (linear)
    triple_cov_lin[[1]],          # [4] pdsi.l1 (linear) + tmin.l1 (linear) + urban (linear)
    multi_cov_nl_rep[[7]],        # [5] pdsi.l1 (nonlinear replicated by 'main_climate_f') + tmin.l1 (linear)
    interacting_cov[[1]],         # [6] pdsi.l1, tmin.l1, urban (linear main effects + 2-way & 3-way interactions) 
    varying_cov[[1]]              # [7] pdsi.l1 (linear varying by 'main_climate_f') + tmin.l1 (linear)
  ),  
  uni_cov_nl[7:12]                # [8–13] pdsi.l1 through pdsi.l6 (nonlinear) 
)



## ----formulas_cov_list--------------------------------------------------------
formulas_cov_list <- write_inla_formulas(
  outcome = "dengue_cases",
  covariates = cov_list,
  re1 = list(id ="month_id",
             model ="rw1", cyclic = TRUE,
             hyper = "prior_t",
             replicate = "spat_meso_id" ),
  re2 = list(id = "year_id",
             model = "iid",
             hyper = "prior_t"),
  re3 = list(id = "spat_id",
             model = "bym2",
             graph = "g", 
             hyper = "prior_sp"),
  baseline = TRUE)

# Example of INLA formula generated
formulas_cov_list[1]

class(formulas_cov_list)

## ----formulas_cov_list_ghr----------------------------------------------------
formulas_cov_list_ghr <- as_GHRformulas(formulas = formulas_cov_list)

class(formulas_cov_list_ghr)

## ----fit_models model_list, eval = FALSE--------------------------------------
# 
# model_cov_list <- fit_models(
#   formulas = formulas_cov_list_ghr,
#   data = data,
#   family = "nbinomial",           # Negative binomial likelihood
#   name = "mod",                   # Label prefix for each model
#   offset = "population",          # Offset variable to account for population size
#   control_compute = list(
#     config = FALSE,               # Do not posterior predictive distribution
#     vcov = FALSE                  # Do not return variance-covariance matrix
#   ),
#   pb = TRUE,                      # Display progress bar
#   nthreads = 8                    # Use 8 threads for parallel computation
# )
# 
# class(model_cov_list)
# 
# model_cov_list_gof <- model_cov_list$mod_gof

## ----fit_models model_list readrds, echo = FALSE------------------------------
model_cov_list <- readRDS(
  system.file("examples", "model_cov_list.rds", package = "GHRmodel")
)

model_cov_list_gof <- model_cov_list$mod_gof

## ----plot interactions,  fig.width = 6, fig.height = 4------------------------
# Plot linear effects and their interactions
plot_coef_lin(
  model = model_cov_list,                      # A list of fitted INLA models
  mod_id = c("mod2", "mod4", "mod5", "mod7"),  # Select models with linear effects to be plotted
  # Custom labels for variables (applied only to non-interacting fixed effects)
  var_label = c(
    "tmin.l1" = "Min. temp lag 1",              # Rename 'tmin.l1' to a descriptive label
    "pdsi.l1" = "PDSI lag 1",                   # Rename 'pdsi.l1' to a descriptive label
    "urban"   = "Prop. urban population"        # Rename 'urban' to a descriptive label
  ),
  
  title = "Effects of linear and interacting covariates"  
  # Title for the plot summarizing what is being visualized
)

## ----Köppen-Geiger climate regimes, echo = FALSE------------------------------
knitr::kable(
  data.frame(
    `main_climate_f` = c("1", "2", "3", "4"),
    `main_climate` = c("AF", "AM", "AW", "CFA"),
    Description = c(
      "Tropical Rainforest Climate",
      "Tropical Monsoon Climate",
      "Tropical Savanna Climate with Dry Winter",
      "Humid Subtropical Climate"
    )
  ),
  col.names = c("main_climate_f", "main_climate", "Climate Zone Description"),
  caption = "Main Köppen-Geiger Climate Regimes used in the varying coefficient analysis"
)


## ----Varying coefficients,  fig.width = 8, fig.height = 3, out.width='100%'----
# Plot linear slopes varying by climate zone. 
plot_coef_varying(
  models = model_cov_list, # A list of fitted INLA model objects
  mod_id = "mod8",  # Select the model with varying slopes
  palette = "Blues",   # Color palette for the plot (from RColorBrewer)
  name = "main_climate_f", # The grouping variable (factor) 
  title = "Effect of PDSI at one-month lag for each climate zone",  # Plot title
  ylab = "Main climate zones",  # Label for the y-axis (groups/climate zones)
  unit_label = c( # Map factor levels to descriptive names 
    "1" = "Tropical Rainforest Climate", 
    "2" = "Tropical Monsoon Climate", 
    "3" = "Tropical Savanna Climate with Dry Winter",
    "4" = "Humid Subtropical Climate"
  )
)

## ----plot_coef_nl grid replicated,  fig.width = 8, fig.height = 8, out.width='100%'----
# PLot replicated nonlinear effects
plot_coef_nl(
  models = model_cov_list, # List of fitted INLA model objects
  mod_id = c("mod3", "mod6"), # Select which models to include in the plot
  mod_label = c( # Custom display labels for the selected models
    "mod3" = "pdsi.l1_rep_clim",    
    "mod6" = "pdsi.l1_rep_clim + tmin.l1"
  ),
  var_label = c( # Rename variables for clearer axis/legend labels
    "pdsi.l1" = "PDSI lag 1"
  ),
  name = "pdsi.l1", # Variable to plot: nonlinear effect of pdsi.l1
  title = "Nonlinear effect of PDSI at one-month lag replicated by main climate", 
  xlab = "PDSI", # X-axis label
  palette = "IDE2", # Color palette for plotting the nonlinear curves
  collapse = FALSE, # Display results in a grid (one plot per covariate-model pair)
  rug = FALSE,  # Do not show rug plot for data density along the x-axis
  histogram = TRUE, # Show histogram of covariate distribution instead of rug plot
  legend = "Climate zone" # Add legend title
)

## ----formulas_user_ghr--------------------------------------------------------
# Convert list of user-defined INLA formulas into a GHRformulas object 
formulas_user_ghr <- as_GHRformulas(c(
  
  # Model 1: random effects only, where monthly random effect is replicated by meso region and the spatial random effect is replicated by year
    "dengue_cases ~ 1 +
     f(month_id, model = 'rw1', replicate = spat_meso_id, cyclic = TRUE, constr = TRUE, hyper = prior_t) +
     f(year_id, model = 'iid', constr = TRUE, hyper = prior_t) +
     f(spat_id, model = 'bym2', graph = g, constr = TRUE, hyper = prior_sp, replicate = year_id2)",
    
  # Model 2: random effects and a varying effect for pdsi lag 1 by climate zone
  "dengue_cases ~ 1 + f(main_climate_f, pdsi.l1, model = 'iid') +
     f(month_id, model = 'rw1', replicate = spat_meso_id, cyclic = TRUE, constr = TRUE, hyper = prior_t) +
     f(year_id, model = 'iid', constr = TRUE, hyper = prior_t) +
     f(spat_id, model = 'bym2', graph = g, constr = TRUE, hyper = prior_sp, replicate = year_id2)",

  # Model 3: random effects and a 3-way interaction between different pdsi and tmin lags
  "dengue_cases ~ 1 + pdsi.l1 + tmin.l3 + pdsi.l6 + pdsi.l1:tmin.l3:pdsi.l6 +
     f(month_id, model = 'rw1', replicate = spat_meso_id, cyclic = TRUE, constr = TRUE, hyper = prior_t) +
     f(year_id, model = 'iid', constr = TRUE, hyper = prior_t) +
     f(spat_id, model = 'bym2', graph = g, constr = TRUE, hyper = prior_sp, replicate = year_id2)"
))

# Visualize output: GHRformulas object
class(formulas_user_ghr)

## ----fit_models model_user, eval = FALSE--------------------------------------
# # User-defined INLA-compatible formulas can be passed into fit_models() as a GHRformulas object
# model_user <- fit_models(
#   formulas = formulas_user_ghr,
#   data = data,
#   family = "nbinomial",           # Negative binomial likelihood
#   name = "mod",                   # Label prefix for each model
#   offset = "population",          # Offset variable to account for population size
#   control_compute = list(
#     config = FALSE,               # Do not posterior predictive distribution
#     vcov = FALSE                  # Do not return variance-covariance matrix
#   ),
#   pb = TRUE,                      # Display progress bar
#   nthreads = 8                    # Use 8 threads for parallel computation
# )

## ----fit_models model_user readrds, echo=FALSE--------------------------------
model_user<- readRDS(system.file("examples", "model_user.rds",
                                 package = "GHRmodel"))

model_user_gof <- model_user$mod_gof 

## ----plot user model,fig.width = 6, fig.height = 3----------------------------
# Plot any linear coefficients found in the fitted model results. 
plot_coef_lin(
  model = model_user,              # Provide fitted model GHRmodels object
  exp = TRUE,                      # Exponentiate coefficients to relative risk scale
  title = "Relative Risk (RR)"     # Plot title
)

