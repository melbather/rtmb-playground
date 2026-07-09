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

# Starting with a single session

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

# See how it looks
plot(mask)
points(detectors, col = "red", pch = 19)

# function to simulate wiggly surface
wiggly_surface <- function(mask, cov.pars, beta0, seed) {
  set.seed(seed)
  exp(
    geoR::grf(
      nrow(mask),
      grid = mask, 
      xlims = range(mask$x),
      ylims = range(mask$y), 
      cov.pars = cov.pars
    )$data + beta0
  )
}

# TODO 
# run the below in a loop and track which seed is the best one
# then run the model
# add ability to plotting function to show animal coords
# add ability to plotting function to show detectors, by size of number of detections
# add option for user to pass in upper limit for sigma into model function

# simulate different surfaces with different parameters
D1 <- wiggly_surface(mask, c(3, 100000*0.5), 0, 42)
D2 <- wiggly_surface(mask, c(3, 100000*0.5), -1, 43)
D3 <- wiggly_surface(mask, c(4, 100000*0.5), -2, 44)
D4 <- wiggly_surface(mask, c(4, 100000*0.5), 1, 45)
D5 <- wiggly_surface(mask, c(2, 100000*0.5), -2, 46)
D6 <- wiggly_surface(mask, c(5, 100000*0.5), -2, 47)
D7 <- wiggly_surface(mask, c(2, 100000*0.5), -1, 48)
D8 <- wiggly_surface(mask, c(3, 100000*0.2), -2, 49)
D9 <- wiggly_surface(mask, c(4, 100000*0.2), -2, 50)
D10 <- wiggly_surface(mask, c(4, 100000*0.1), -3, 51)
D11 <- wiggly_surface(mask, c(4, 100000*0.3), -1, 52)
D12 <- wiggly_surface(mask, c(2, 100000*0.1), -3, 53)
D13 <- wiggly_surface(mask, c(2, 100000*0.1), -1, 54)
D14 <- wiggly_surface(mask, c(2, 100000*0.1), -2, 55)
D15 <- wiggly_surface(mask, c(20, 100000*0.1), -2, 56)
D16 <- wiggly_surface(mask, c(20, 100000*0.1), -2, 57)
D17 <- wiggly_surface(mask, c(20, 100000*0.1), -2, 58)
D18 <- wiggly_surface(mask, c(20, 100000*0.1), 1, 58)
D19 <- wiggly_surface(mask, c(20, 100000*0.025), 1, 60)
D20 <- wiggly_surface(mask, c(20, 100000*0.025), -9.5, 60)

plot.surf(mask$x, mask$y, D1, detectors)
plot.surf(mask$x, mask$y, D2, detectors)
plot.surf(mask$x, mask$y, D3, detectors)
plot.surf(mask$x, mask$y, D4, detectors)
plot.surf(mask$x, mask$y, D5, detectors)
plot.surf(mask$x, mask$y, D6, detectors)
plot.surf(mask$x, mask$y, D7, detectors)
plot.surf(mask$x, mask$y, D8, detectors)
plot.surf(mask$x, mask$y, D9, detectors)
plot.surf(mask$x, mask$y, D10, detectors)
plot.surf(mask$x, mask$y, D11, detectors)
plot.surf(mask$x, mask$y, D12, detectors)
plot.surf(mask$x, mask$y, D13, detectors)
plot.surf(mask$x, mask$y, D14, detectors)
plot.surf(mask$x, mask$y, D15, detectors)
plot.surf(mask$x, mask$y, D16, detectors)
plot.surf(mask$x, mask$y, D17, detectors)
plot.surf(mask$x, mask$y, D18, detectors)
plot.surf(mask$x, mask$y, D19, detectors)
plot.surf(mask$x, mask$y, D19, detectors)
plot.surf(mask$x, mask$y, D20, detectors)

# Use old simulation code from dissertation (modified)
animals_in_cells <- rpois(length(D20), mask_area * D20)
  
#ignore mask cells where there are no animals
zero_animal_cells <- which(animals_in_cells == 0)
#edge case for if every single cell has an animal in it
if(length(zero_animal_cells) == 0) {
  populated_cells <- mask
} else {
  populated_cells <- mask[-zero_animal_cells,]
  animals_in_cells <- animals_in_cells[-zero_animal_cells]
}

total_animals <- sum(animals_in_cells)


#FIND ANIMAL POSITIONINGS WITHIN MASK CELLS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#x and y displacements for all animals
#find how far each animal is from the centre of their cell
x_displacements <- runif(total_animals, -0.5*mask_length_x, 0.5*mask_length_x)
y_displacements <- runif(total_animals, -0.5*mask_length_y, 0.5*mask_length_y)

mask_x <- rep(populated_cells[,1], times = animals_in_cells)
mask_y <- rep(populated_cells[,2], times = animals_in_cells)

#coords of all the animals
animal_x <- mask_x + x_displacements
animal_y <- mask_y + y_displacements
animal_coords <- cbind(animal_x, animal_y)

#DISTANCES OF ANIMALS FROM DETECTORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#find distances of all animals to all the detectors
distances <- fields::rdist(animal_coords, detectors)

#hence find their detection probabilities
g0 <- 0.5
sigma <- 70
probabilities <- g0 * exp(-distances^2/(2*sigma^2)) 
random_capture_hist <- matrix(rbinom(length(probabilities), 1, probabilities), nrow(probabilities), ncol(probabilities)) 
random_capture_hist_no_zero <- random_capture_hist[which(rowSums(random_capture_hist) != 0),]

fit_sim <- fit_scr(
  list(random_capture_hist_no_zero),
  list(mask),
  mask,
  list(detectors),
  "~s(x, y, k=25)",
  "HN"
)

summary(fit_sim$sdreport)

# plot density
# overlay with activity centres]

# prediction mask
pred_mask <- expand.grid(
  x = 1:110,
  y = 1:110
)

pred_detectors <- matrix(c(rep(1:10, each = 10)*10, rep(1:10, 10)*10), ncol=2)

plot_density(fit_sim, pred_mask, detectors = NULL)
points(animal_coords, col="black")
