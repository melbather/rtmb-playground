# function to generate total animals + the locations of their activity centres
sim_activity_centres <- function(D, mask, mask_area) {
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

  list(
    animal_coords = animal_coords,
    total_animals = total_animals
  )
}