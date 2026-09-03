# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(mgcv)
library(dplyr)
library(fields)

# Get model functions to work using the data from sim_multi_species.R
load("sim data new/multi_species_sim_data.RData")

# First, need to put data into format that fit_scr needs
# only one session for this
bin_capt <- list(
  session1 <- list(
    species1 = random_capture_hist_no_zero1,
    species2 = random_capture_hist_no_zero2
  )
)

fit_scr(
  bin_capt, 
  list(mask), 
  mask, 
  list(detectors), 
  formula = "~s(x, y, k=25)",
  num_species = 2, 
  num_fields = 2
)
