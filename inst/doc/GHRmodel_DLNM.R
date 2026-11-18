## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----Load required libraries, eval=FALSE--------------------------------------
# # Load necessary package dependencies
# library(dplyr)        # Data manipulation
# library(ggplot2)      # Data visualization: creating plots and graphs
# library(sf)           # Handling spatial vector data (simple features)
# library(spdep)        # Spatial dependence and autocorrelation analysis
# library(INLA)         # Bayesian inference
# library(dlnm)         # Distributed linear and nonlinear models
# 
# # Load GHRmodel
# library(GHRmodel)

## ----Load required libraries invisible, echo=FALSE----------------------------
# Load necessary package dependencies
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization: creating plots and graphs
library(sf)           # Handling spatial vector data (simple features)
library(spdep)        # Spatial dependence and autocorrelation analysis
# library(INLA)         # Bayesian inference
library(dlnm)         # Distributed linear and nonlinear models

# Load GHRmodel
library(GHRmodel)

## ----Process the data set-----------------------------------------------------
# Load data
data("dengue_MS")
df <- dengue_MS

# Create ID variables
df <- df |>
  # Filter out the year 2000
  filter(year > 2000) |>
  # Create numeric IDs for year, month and various spatial units.
  mutate(
    year_id = as.numeric(as.factor(year)),
    month_id = as.numeric(as.factor(month)),
    spat_id = as.numeric(as.factor(micro_code)),
    spat_meso_id = as.numeric(as.factor(meso_code))
  )

## ----Load map_MS--------------------------------------------------------------
# Load map included in package
data("map_MS")

# Create adjacency Matrix
nb <- spdep::poly2nb(map_MS)
g <- spdep::nb2mat(nb, style = "B")

## ----Creating a onebasis for tmin---------------------------------------------
# Place 3 internal knots at the 25th, 50th, and 95th percentiles of tmin.
tminknots <- quantile(df$tmin, c(25, 50, 95) / 100, na.rm = T)

# Construct a one-dimensional basis for modeling tmin.
ob_tmin <- onebasis_inla(
  covariate   = df$tmin,   # Covariate values to transform.
  fun         = "bs",      # Basis function type: B-spline.
  knots       = tminknots, # Internal knots for the spline
  degree      = 3,         # Degree of each spline piece (cubic)
  basis_name  = "tmin"     # Prefix for the generated basis matrix column names.
)

# Display the first few rows of the basis matrix to inspect the transformation.
head(ob_tmin)

## ----Create lagged matrices for tmin and pdsi---------------------------------
# Lagged covariate matrix for tmin
lagged_tmin <- lag_cov(
  data = df,
  name = "tmin",
  time = "date",
  lag = 1:2,         # 1 to 2-month lags
  group = "micro_code", # Independent time-series for each microregion
  add = FALSE
)

# Lagged covariate matrix for pdsi
lagged_pdsi <- lag_cov(
  data = df,
  name = "pdsi",
  time = "date",
  lag = 0:2,         # 0 to 2-month lags
  group = "micro_code", # Independent time-series for each microregion
  add = FALSE
)

## ----Creating a crossbasis for tmin, eval=FALSE-------------------------------
# # 2-dimensional nonlinear effect of dengue risk across tmin values and lags
# cb_tmin <- crossbasis_inla(
#   covariate = lagged_tmin,   # Matrix of lagged tmin values
#   lag = c(1, 2),             # Lags from 1 to 2 months
#   basis_name = "tmin",       # Prefix for output column names
#   argvar = list(fun = "bs"), # Default B-spline (cubic) for exposure-response
#   arglag = list(
#     fun = "ns", # Natural cubic splines for lag-response
#     knots = logknots(
#       2,     # Maximum lag (here 2)
#       nk = 1 # Number of knots
#     )
#   )
# )

## ----load cb_tmin, echo = FALSE-----------------------------------------------
cb_tmin <- readRDS(system.file("examples", "cb_tmin.rds", package = "GHRmodel"))

## ----inspect cb_tmin----------------------------------------------------------
# Inspect the resulting matrix
dim(cb_tmin)
colnames(cb_tmin)

## ----Creating a crossbasis for pdsi-------------------------------------------
# 2-dimensional nonlinear effect of dengue risk across pdsi values and lags
cb_pdsi <- crossbasis_inla(
  covariate = lagged_pdsi,
  lag = c(0, 2),
  basis_name = "pdsi",
  argvar = list(fun = "bs"), # Default B-spline for exposure-response
  arglag = list(fun = "bs")  # Default B-spline for lag-response
)

## ----Define monthly and yearly random effects priors--------------------------
# Define Gamma priors for the precision of temporal random effects
prior_t <- list(prec = list(prior = "loggamma", param = c(0.01, 0.01)))

## ----Define spatial random effect priors--------------------------------------
# Define penalized complexity (PC) priors for spatial random effects using BYM2
prior_sp <- list(
  prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01)), # Precision of spatial effect
  phi  = list(prior = "pc", param = c(0.5, 2 / 3))             # Mixing parameter
)

## ----Write DLNM INLA-compatible model formulas--------------------------------
# Write INLA-compatible model formulas including DLNM terms
formulas_dlnm <- write_inla_formulas(
  outcome = "dengue_cases",
  covariates = list(
    c("ob_tmin"),           # Model 2: includes one-basis object (tmin)
    c("cb_tmin"),           # Model 3: includes cross-basis for tmin
    c("cb_tmin", "cb_pdsi") # Model 4: includes cross-basis for tmin and pdsi
  ),
  baseline = TRUE,          # Model 1: random effects only (default: TRUE)
  # First random effect: monthly seasonality
  re1 = list(
    id        = "month_id",    # Time index for monthly effect
    model     = "rw1",         # First-order random walk model
    cyclic    = TRUE,          # Enforce cyclicity over months
    hyper     = "prior_t",     # Hyperprior object for precision
    replicate = "spat_meso_id" # Separate seasonal pattern per mesoregion
  ),
  # Second random effect: inter-annual trend
  re2 = list(
    id    = "year_id", # Year index
    model = "iid",     # Independent and identically distributed
    hyper = "prior_t"  # Hyperprior for temporal smoothness
  ),
  # Third random effect: spatial autocorrelation
  re3 = list(
    id    = "spat_id", # Spatial unit identifier
    model = "bym2",    # BYM2 spatial model (structured + unstructured)
    graph = "g",       # Pre-computed spatial adjacency graph
    hyper = "prior_sp" # PC priors for BYM2 model parameters
  )
)

## ----Fit DLNM INLA model, eval = FALSE----------------------------------------
# # Fitting a DLNM model using a previously defined formula and basis objects
# model_dlnm <- fit_models(
#   formulas = formulas_dlnm,
#   name = "mod",
#   data = df,
#   family = "nbinomial",
#   offset_var = "pop100000",
#   control_compute = list(config = TRUE, vcov = TRUE),
#   progress = TRUE
# )

## ----fit_models model_dlnm readrds, echo=FALSE--------------------------------
model_dlnm <- readRDS(system.file("examples", "model_dlnm.rds", package = "GHRmodel"))

## ----print model_dlnm---------------------------------------------------------
model_dlnm

## ----model_dlnm_gof, echo=FALSE-----------------------------------------------
# Extract goodness of fit metrics as a data.frame
model_dlnm_gof <- model_dlnm$mod_gof
options(knitr.kable.NA = "")
knitr::kable(head(model_dlnm_gof[, c(1:3)]),
  caption = "DLNM terms in `model_dlnm`"
)

## ----cpred_tmin---------------------------------------------------------------
# Generate dengue risk predictions for a range of tmin values.
cpred_ob_tmin <- crosspred_inla(
  models = model_dlnm, # GHRmodels object
  basis = ob_tmin,     # One-basis object for the `tmin` variable
  mod_id = "mod2",     # Identifier to select the correct model component
  cen = 19,            # Temperature centered at the mean of tmin 19°C
  from = 15,           # Predictions are generated from 15°C ...
  to = 24              # ... up to 24°C
)

# Output a GHRcrosspred object
class(cpred_ob_tmin)

## ----plot_coef_crosspred, fig.width = 6, fig.height = 4-----------------------
# Plot the predicted dengue risk values against the tmin values
plot_coef_crosspred(
  cpred_ob_tmin,    # The cross-predicted object from crosspred_inla()
  type = "slices",              # Plot type: shows effect as a continuous curve
  var = seq(15, 24),            # Range of tmin values to display on the x-axis
  line_color = "red",         # Color of the fitted effect line
  line_size = 0.8,              # Thickness of the effect line
  ribbon_color = "red",       # Color of the credible interval (ribbon)
  ribbon_alpha = 0.3,           # Transparency level of the ribbon
  title = "Effect of minimum temperature on dengue risk", # Plot title
  xlab = "Minimum temperature", # Label for the x-axis
  ylab = "Effect"               # Label for the y-axis
)

## ----Evaluate coefficients for cb_tmin----------------------------------------
# Generate dengue risk predictions for a range of tmin values and lags.
cpred_cb_tmin <- crosspred_inla(
  models = model_dlnm, # GHRmodels object
  basis = cb_tmin,     # Cross-basis from tmin
  mod_id = "mod3",     # Model identifier
  cen = 19,            # Centering value (mean minimum temperature, 19°C)
  from = 17,           # Start predicting at
  to = 24
)

# Output a GHRcrosspred object
class(cpred_cb_tmin)

## ----Plot coefficients for cb_tmin slices by lag, fig.width = 7, fig.height = 4----
# Plot the predicted dengue risk values against the tmin values and lags
plot_coef_crosspred(
  crosspred = cpred_cb_tmin, # Crosspred object with model predictions
  type = "slices",           # Plot lag-specific slices of exposure-response curves
  exp = TRUE,                # Exponentiate the coefficients,
  lag = 1:2,                 # Display results for lags 1 and 2
  line_color = "red",        # Red color for the lines representing effect estimates
  line_size = 0.8,           # Line thickness set to 0.8 for better visibility
  ribbon_color = "red",      # Red shading for credible interval ribbons
  ribbon_alpha = 0.3,        # Set ribbon transparency to 30%
  title = "Effect of minimum temperature on dengue relative risk by lag", 
  xlab = "Mean minimum temperature exposure", # Label for the x-axis (exposure variable)
  ylab = "Relative Risk (RR)"                 # Label for the y-axis (effect estimate scale)
)

## ----Plot coefficients for cb_tmin slices by temp values,  fig.width = 7, fig.height = 5----
# Plot the predicted dengue risk values against the tmin values and lags
plot_coef_crosspred(
  crosspred = cpred_cb_tmin,  # Crosspred object with model predictions
  type = "slices",            # Plot temperature-specific slices of exposure-response curves
  exp = TRUE,                 # Exponentiate the coefficients (to relative risk scale)
  var = c(20:24),             # Display results for temperature 20°C to 24°C
  line_color = "red",         # Red color for the lines representing effect estimates
  line_size = 0.8,            # Line thickness set to 0.8 for better visibility
  ribbon_color = "red",       # Red shading for credible interval ribbons
  ribbon_alpha = 0.3,         # Set ribbon transparency to 30%
  title = "Effect of minimum temperatures 20°C to 24°C on dengue relative risk by lag", # Main plot title
  xlab = "Lag",               # Label for the x-axis (exposure variable)
  ylab = "Relative Risk (RR)" # Label for the y-axis (effect estimate scale)
)

## ----Plot coefficients for cb_tmin heatmap limits, fig.width = 6, fig.height = 5----
# Same heatmap with fixed colour scale limits
plot_coef_crosspred(
  crosspred = cpred_cb_tmin,
  type = "heatmap",
  exp = TRUE,
  heat_lims = c(0.5, 2),     # Colour scale limits on the relative risk scale
  palette = "-RdBu",
  title = "Effect of minimum temperature on dengue relative risk by lag"
)

## ----Plot coefficients for cb_tmin heatmap, fig.width = 6, fig.height = 5-----
# Plot the predicted dengue risk values summed across lags
plot_coef_crosspred(
  crosspred = cpred_cb_tmin, # Crosspred object containing model predictions
  type = "heatmap",          # Heatmap of effects over exposure and lag values
  exp = TRUE,                # Exponentiate effects to show relative risks
  palette = "-RdBu",         # Use the inverted "RdBu" color palette
  title = "Effect of minimum temperature on dengue relative risk by lag"
)

## ----Plot coefficients for cb_tmin heatmap smooth, fig.width = 6, fig.height = 5----
# Generate predictions using a higher-resolution range of temperature values
cpred_cb_tmin_smooth <- crosspred_inla(
  models = model_dlnm,         # GHRmodels object
  basis = cb_tmin,             # Cross-basis from tmin
  mod_id = "mod3",             # Model identifier
  cen = 19,                    # Centering value (mean minimum temperature 19°C)
  at = seq(17, 24, by = 0.05), # Increased number of points for prediction
  from = 17,
  to = 24
)

# Plot a granular heatmap with increased number of values for prediction
# and increased number of lag interpolation values
plot_coef_crosspred(
  crosspred = cpred_cb_tmin_smooth, # Crosspred object
  type = "heatmap",                 # Heatmap of effects over exposure and lag
  exp = TRUE,                       # Exponentiate effects to show RR
  n_lag_smooth = 200,               # Smooth across lag with 200 interpolation points
  palette = "-RdBu",                # Use the inverted "RdBu" color palette
  title = "Effect of minimum temperature on dengue relative risk by lag",
  ylab = "Mean minimum temperature exposure" # Label for the y-axis
)

## ----Plot coefficients for cb_tmin overall, fig.width = 6, fig.height = 4-----
# Plot overall effect of tmin on dengue across lags
plot_coef_crosspred(
  crosspred = cpred_cb_tmin,
  type = "overall",
  exp = TRUE,           # Exponentiate effects to show relative risks
  line_color = "Red",   # Red color for the lines representing effect estimates
  line_size = 0.8,      # Line thickness set to 0.8 for better visibility
  ribbon_color = "Red", # Red shading for credible interval ribbons
  ribbon_alpha = 0.3,   # Set ribbon transparency to 30%
  xlab = "Mean minimum temperature exposure", # Label for the x-axis
  ylab = "Effect"       # Label for the y-axis (effect estimate scale)
)

