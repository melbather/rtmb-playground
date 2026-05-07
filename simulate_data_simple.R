# Code taken from MSc dissertation 2023
set.seed(123)

#Load data and libraries -------------------------------------------------------
library(mvtnorm)
library(fields)
library(gstat)
library(CircStats)
library(dplyr)

#Call functions ----------------------------------------------------------------
source("simulation functions/create_villages.R")
source("simulation functions/find_bearings.R")
source("simulation functions/format_capture_histories.R")
source("simulation functions/simple_simulate_capture_histories.R")
source("simulation functions/generate_session_information.R")
source("simulation functions/generate_session_locations.R")
source("simulation functions/create_mask.R")

#Create two villages -----------------------------------------------------------
village_locations1 <- create_villages(2, c(0, 8000), c(0, 700))

#Set number of sessions --------------------------------------------------------
num_sessions1 <- 3

#Generate session information --------------------------------------------------
sessions1 <- generate_session_information(num_sessions1, 
                                         c("rain", "sun", "snow", "overcast"), 
                                         c("mountain", "non-mountain"), 
                                         2012:2018, 1000, 1000, 40)

#Create a mask -----------------------------------------------------------------
mask1 <- create_mask(600, 600, 66, 66)

#Set detector locations --------------------------------------------------------
detectors1 <- matrix(c(rep(-1:1, each = 3)*100, rep(-1:1, 3)*100), ncol=2)

#Test with the first session only ----------------------------------------------
single_session_demo <- simple_simulate_capture_histories(
  session_info = sessions1[1,], 
  alpha_forest = 400, 
  alpha_protected = 300, 
  alpha_altitude = 400, 
  beta0 = -1, beta1 = -0.0002, 
  mask_locations = mask1, 
  detector_locations = detectors1, 
  g0_base = 0.85, 
  sigma = 85, 
  x_range = c(sessions1[1,5], sessions1[1,6]), 
  y_range = c(sessions1[1,7], sessions1[1,8]), 
  beta2 = -0.0002, beta3 = 2.5, beta4 = 0.00015, 
  beta5 = 0.5, beta6 = -0.2, beta7 = 1.9, 
  beta8 = -0.00001, beta9 = 0.000002,
  village_locations = village_locations1, 
  cp_beta0 = log(50), cp_beta1 = -1)

# Overwrite the forest component so that half the area is "forest"
single_session_demo$forest <- c(rep(1, 1650), rep(0, 1650))

#Save workspace image for later use --------------------------------------------
save.image("simulated_data_simple.RData")
