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
  alpha0 = -4,
  alpha1 = 0.7,
  alpha2 = 0.8,
  seed1 = 2,
  seed2 = 3
)

# simulate locations of activity centres for each species
species1 <- sim_activity_centres(D$D1, mask, mask_area)
species2 <- sim_activity_centres(D$D2, mask, mask_area)

# plot the coordinates for both species

#DISTANCES OF ANIMALS FROM DETECTORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#find distances of all animals to all the detectors
distances1 <- fields::rdist(species1$animal_coords, detectors)
distances2 <- fields::rdist(species2$animal_coords, detectors)

#hence find their detection probabilities
g0_1 <- 0.5
sigma_1 <- 70
probabilities1 <- g0_1 * exp(-distances1^2/(2*sigma_1^2)) 

g0_2 <- 0.4
sigma_2 <- 75
probabilities2 <- g0_2 * exp(-distances2^2/(2*sigma_2^2)) 

random_capture_hist1 <- matrix(rbinom(length(probabilities1), 1, probabilities1), nrow(probabilities1), ncol(probabilities1)) 
random_capture_hist_no_zero1 <- random_capture_hist1[which(rowSums(random_capture_hist1) != 0),]

random_capture_hist2 <- matrix(rbinom(length(probabilities2), 1, probabilities2), nrow(probabilities2), ncol(probabilities2)) 
random_capture_hist_no_zero2 <- random_capture_hist2[which(rowSums(random_capture_hist2) != 0),]

# plot coordinates of both species
plot(species1$animal_coords, col="red", pch=19)
points(species2$animal_coords, col="blue", pch=19)

save.image("sim data new/multi_species_sim_data.RData")
