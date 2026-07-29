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

# Run the function above on a loop and save output in a list
n_sim <- 2
wiggly_surface_output <- vector(mode = "list", length = n_sim)
setNames(wiggly_surface_output, paste0("D", 1:n_sim))

for (i in 1:n_sim) {
  name <- paste0("D", i)
  wiggly_surface_output[[i]] <- wiggly_surface(mask, c(30, 100000*0.02), -10, i)
  plot.surf(mask$x, mask$y, wiggly_surface_output[[i]], detectors, title = name)
}

save.image("wiggly_surfaces_run2.RData")

# Load the previously saved simulation above
load("wiggly_surfaces_run2.RData")

D <- wiggly_surface_output[[2]]

# Use old simulation code from dissertation (modified)
animals_in_cells <- rpois(length(D), mask_area * D)
  
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

# Take a sample of 100 of these since there are too many
# TODO make this conditional on the number of total animals
animal_coords <- animal_coords[sample(nrow(animal_coords), 100),]

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
  x = 1:1900,
  y = 1:1900
)

pred_detectors <- matrix(c(rep(5:14, each = 10)*100, rep(5:14, 10)*100), ncol=2)

plot_density(
  fit_sim, 
  pred_mask, 
  detectors = list(pred_detectors),
  animal_coords = animal_coords,
  plot_det_density = TRUE
)
