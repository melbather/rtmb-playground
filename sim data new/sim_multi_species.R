# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(mgcv)
library(dplyr)
library(fields)

source("fitting-functions/fitting-functions.R")
source("fit_scr.R")
# Source all misc functions
sapply(
  list.files("misc functions", full.names = TRUE, recursive = TRUE), 
  function(i) {
    source(paste0(i))
  })

# Create mask grid
# manually calculate mask based on sigma (5x sigma buffer)
mask <- expand.grid(
  x = seq(0, 1900, by = 20),
  y = seq(0, 1900, by = 20)
)

# Get area of mask cell
mask_length_x <- abs(mask[1,1]-mask[2,1])
first_y <- mask[1,2]
second_y <- mask |> 
  as.data.frame() |>                                              
  filter(y != first_y) |> 
  slice(1) |> 
  pull(y)
mask_length_y <- abs(first_y-second_y)
# adjust to hectares
mask_area <- mask_length_x*mask_length_y/10000

attr(mask, "area") <- mask_area

# Detector locations
detectors <- matrix(c(rep(5:14, each = 10)*100, rep(5:14, 10)*100), ncol=2)

D <- wiggly_surface_multi(
  mask,
  c(30, 100000*0.02), 
  beta0 = -10,
  beta1 = 0.9,
  alpha0 = -12,
  alpha1 = 0.7,
  alpha2 = 0.8,
  seed1 = 2,
  seed2 = 3
)

# simulate locations of activity centres for each species
species1 <- sim_activity_centres(D$D1, mask, mask_area)
species2 <- sim_activity_centres(D$D2, mask, mask_area)
