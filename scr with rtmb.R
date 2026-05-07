# SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(acre)

# Load simulated data
# Using simplified data where half the area is forest and half is not forest
load("data/simulated_data_simple.RData")

# Load likelihood function
source("scr_likelihood.R")

# I think there was a reason why I made some of the covariates strings instead of ints in the simulation
# but I can't remember what it was
# They need to be ints to work in this script
single_session_demo$forest <- as.numeric(single_session_demo$forest)
single_session_demo$protected_areas <- as.numeric(single_session_demo$protected_areas)

# PARAMETERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Starting parameters
# For homogeneous case we don't have covariates since density is constant
parameters_scr_homogeneous <- list(
  log_D = log(0.1),
  logit_g0 = qlogis(0.5),
  log_sigma = log(100)
)
# Add covariates to inhomogeneous params
# Only using forest coverage as a covariate for now
parameters_scr_inhomogeneous <- list( 
  logit_g0 = qlogis(0.5),
  log_sigma = log(50),
  beta0 = 0, # intercept term
  beta3 = 0 # coefficient for forest
)

# Create global variable for homogeneous/inhomogeneous
# This is not a very slick way to do it, but seems like the easiest way for now 
# because it looks like the likelihood function can only take in one argument when using MakeADFun
homogeneous <- TRUE

# HOMOGENEOUS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# homogeneous is set to TRUE above
# Test function
scr_likelihood(parameters_scr_homogeneous)

# Convert likelihood function into object that the optimiser can use 
obj_scr_homogeneous <- MakeADFun(scr_likelihood, parameters_scr_homogeneous)
opt_scr_homogeneous <- nlminb(obj_scr_homogeneous$par, obj_scr_homogeneous$fn, obj_scr_homogeneous$gr)
summary(sdreport(obj_scr_homogeneous)) 


# INHOMOGENEOUS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
homogeneous <- FALSE

# Test function again
scr_likelihood(parameters_scr_inhomogeneous)

obj_scr_inhomogeneous <- MakeADFun(scr_likelihood, parameters_scr_inhomogeneous)
opt_scr_inhomogeneous <- nlminb(obj_scr_inhomogeneous$par, obj_scr_inhomogeneous$fn, obj_scr_inhomogeneous$gr)
inhomogeneous_summary <- summary(sdreport(obj_scr_inhomogeneous))


# COMPARE TO ACRE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (Using MSc code)

# Create covariates data frame --------------------------------------------------
cov_df <- data.frame(x = single_session_demo$mask$x,
                     y = single_session_demo$mask$y,
                     forest = as.vector(t(single_session_demo$forest)))

single_session_demo$long_form_capture_history <- single_session_demo$long_form_capture_history[,-5]

# Create an acre object ---------------------------------------------------------
acre_object <- read.acre(single_session_demo$long_form_capture_history, single_session_demo$traps, 
                          control.mask = list(buffer = 85*5),
                          loc.cov = cov_df, 
                          dist.cov = list(villages = village_locations1))

#Create a full model  ----------------------------------------------------------
acre_model_forest <- fit.acre(acre_object, 
                          model = list(D=~forest))

#Summary output ----------------------------------------------------------
summary(acre_model_forest)


# Compare denisty between acre and my code above
# (This only works if ADREPORT(D) is uncommented in the likelihood function)
inhomogeneous_summary |> 
  as.data.frame() |> 
  filter(str_detect(row.names(inhomogeneous_summary), "D")) |> 
  pull(Estimate) |> 
  unique()

predict(acre_model_forest, newdata = data.frame(forest = c(1, 0)))
