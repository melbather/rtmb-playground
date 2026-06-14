# SETUP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(acre)
library(mgcv)
library(ggplot2)
library(dplyr)

# Load simulated data
# Using simplified data where half the area is forest and half is not forest
load("data/simulated_data_simple_constantg0.RData")

# Load functions
source("get_params.R")
source("scr_log_likelihood.R")
source("misc functions/mask_cell_area.R")

# HOMOGENEOUS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test function
# scr_likelihood(parameters_scr_homogeneous, single_session_demo, TRUE)

# Parameters
parameters_scr_homogeneous <- list(
  log_D = log(0.1),
  logit_g0 = qlogis(0.5),
  log_sigma = log(100)
)

# Convert likelihood function into object that the optimiser can use 
obj_scr_homogeneous <- MakeADFun(scr_log_likelihood_closure(scr_log_likelihood, single_session_demo, TRUE), parameters_scr_homogeneous)
opt_scr_homogeneous <- nlminb(obj_scr_homogeneous$par, obj_scr_homogeneous$fn, obj_scr_homogeneous$gr)
summary(sdreport(obj_scr_homogeneous)) 


# INHOMOGENEOUS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dataframe
scr_data_inhomogeneous <- data.frame(
  x = single_session_demo$mask$x, 
  y = single_session_demo$mask$y, 
  forest = single_session_demo$forest
)

# Get params and design matrix
params_and_design <- get_params(scr_data_inhomogeneous, "~ forest + s(x, y, k=20)")

# Pass params and design matrix to log likelihood function
scr_log_likelihood(params_and_design$params, single_session_demo, FALSE, design = params_and_design$design_matrix)

# Try with RTMB using closure
obj_scr_inhomogeneous <- MakeADFun(
  scr_log_likelihood_closure(
    scr_log_likelihood, 
    single_session_demo, 
    FALSE, 
    params_and_design$design_matrix
  ), 
    params_and_design$params, 
    random = "u")
opt_scr_inhomogeneous <- nlminb(obj_scr_inhomogeneous$par, obj_scr_inhomogeneous$fn, obj_scr_inhomogeneous$gr)
sdreport_scr <- summary(sdreport(obj_scr_inhomogeneous))

# Plot densities
estimated_density <- exp(params_and_design$design_matrix$X.lbm %*% sdreport_scr[4:7,1] + params_and_design$design_matrix$Z.lbm %*% sdreport_scr[8:24, 1])

density_df <- data.frame(x = mask1$x,
  y = mask1$y, density = estimated_density)

ggplot(density_df, aes(x, y, z = density)) +
  geom_contour_filled()

hist(estimated_density)

# Test the above process with gibbons data -----------------------------------------------------------
load("data/gibbon-data.RData")

# Get params and design matrix
params_and_design_gibbons <- Map(
  function(mask, mask_df) {
    forest <- ifelse(mask_df$FOREST_COVER == "DENSE", 1, 0)
      df <- data.frame(
      x = mask[,1],
      y = mask[,2],
      forest = forest
    )
    get_params(df, "~ forest + s(x, y, k=20)")  
  },
  mask.full,
  mask.full.df)

# Format data so my function can use it
# gibbons_data <- Map(
#   function(capt, trap, mask) {
#     binary_capture_history <- as.data.frame(capt$bincapt)
#     traps <- as.data.frame(traps)
#     names(traps)[1:2] <- c("x", "y")
#     mask <- as.data.frame(mask)
#     names(mask)[1:2] <- c("x", "y")
#     area <- mask_cell_area(mask)

#     list(
#       binary_capture_history = binary_capture_history,
#       traps = traps,
#       mask = mask,
#       mask_cell_area = area
#     )
#   },
#   capt,
#   traps,
#   mask.full
# )
gibbons_data <- list()
for (i in 1:length(capt)) {
  gibbons_data[[i]] <- list(
    binary_capture_history = as.data.frame(capt$bincapt[[i]]),
    traps = traps[[i]],
    mask = mask.full[[i]],
    mask_cell_area = mask_cell_area(mask.full[[i]])
  )
}

# Apply likelihood function to each session and add the log likelihoods together
gibbons_ll <- sum(
  unlist(Map(
    function(p, data) {
      scr_log_likelihood(
        p[[2]], data, FALSE, p[[1]] # TODO fix this
      )
    },
    params_and_design_gibbons,
    gibbons_data
  ))
)

test <- 0
for (i in 1:58) {
  design_matrix <- params_and_design_gibbons[[i]][["design_matrix"]]
  parameters <- list(
    
  )
  ll <- scr_log_likelihood(parameters, gibbons_data[[i]], FALSE, design_matrix)
  print(ll)
  test <- test + ll
}


# Test that likelihood closure works
scr_log_likelihood(params_and_design_gibbons$params, single_session_demo, FALSE, design = params_and_design_gibbons$design_matrix)



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
