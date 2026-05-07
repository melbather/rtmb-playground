# Code taken from MSc dissertation 2023
# Altered to make half the area forest and half non-forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to simulate capture histories when there is inhomogeneous density 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ARGUMENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#session_info: should be a data frame with one row containing the session ID and covariate information
#alpha_forest: controls how smooth the field is in terms of forest/non-forest
#alpha_protected: same as above but for protected/non-protected areas
#alpha_protected: same as above but for altitude
#beta0: intercept term for density
#beta1: coefficient of x-coordinate for density
#mask_locations: locations of all mask cells
#detector_locations: locations of all detectors
#g0_base: baseline detection probability
#sigma: sigma for probability calculation
#x_range/y_range = all possible values for x and y in the survey region
#beta2: coefficient of y-coordinate for density
#beta3: density coefficient for forest
#beta4: density coefficient for distance to nearest village
#beta5: coefficient to determine effect of weather on baseline capture probability/g0
#beta6: coefficient to account for effect of mountains on baseline capture probability/g0
#beta7: density coefficient for protected areas
#beta8: density coefficient for altitude
#beta9: density coefficient for time
#village_locations: x and y coordinatess of villages
#cp_beta0: value for exp(kappa) (concentration parameter)
#cp_beta1: value to adjust concentration parameter in presence of mountains
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


simple_simulate_capture_histories <- function(session_info, alpha_forest, alpha_protected, alpha_altitude,
                                                     beta0, beta1 = 0, mask_locations, detector_locations, g0_base, 
                                                     sigma, x_range, y_range, beta2 = 0, beta3 = 0, beta4 = 0, 
                                                     beta5 = 0, beta6 = 1, beta7 = 0, beta8 = 0, beta9 = 0,
                                                     village_locations = NULL, cp_beta0, cp_beta1 = 0) {
  
  #MASK AREA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #change location of the mask based on session start location
  mask_locations[,1] <- mask_locations[,1] + x_range[1]
  mask_locations[,2] <- mask_locations[,2] + y_range[1]
  mask_length_x <- abs(mask_locations[1,1]-mask_locations[2,1])

  # Mask cells are not necessarily square
  first_y <- mask_locations[1,2]
  second_y <- mask_locations |> 
    as.data.frame() |>                                              
    filter(y != first_y) |> 
    slice(1) |> 
    pull(y)
  mask_length_y <- abs(first_y-second_y)
  mask_cell_area <- mask_length_x*mask_length_y
  
  #UPDATE TRAP LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #change locations of traps based on session start location
  detector_locations[,1] <- detector_locations[,1] + x_range[1]
  detector_locations[,2] <- detector_locations[,2] + y_range[1]
  
  
  #COMPUTE DISTANCE MATRIX OF ALL MASK LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  A <- as.matrix(dist(mask_locations))
  
  #FOREST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Determine forest locations
  #multivariate normal dist - mask cells are correlated with one another
  sim.forest <- c(rep(1, nrow(mask_locations)/2), rep(0, nrow(mask_locations)/2)) #1 is forest
  
  #PROTECTED LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Determine protected locations
  #multivariate normal dist - mask cells are correlated with one another
  cov_protected <- exp(-A/alpha_protected) 
  sim.rmv.protected <- rmvnorm(1, sigma = cov_protected)
  sim.protected <- c(ifelse(sim.rmv.protected > 0, 1, 0)) #1 is protected area
  #this could be the distance from the protected area
  
  #ALTITUDE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Randomly select points within the survey region/mask area and assign them altitudes
  #add altitude column to mask locations
  mask_locations_with_alt <- as.data.frame(mask_locations)
  #make altitude depend on terrain
  if(session_info$terrain == "mountain") alt_range <- c(2000,4500)
  else alt_range <- c(0,2000)
  cov_altitude <- exp(-A/alpha_altitude)
  mask_locations_with_alt$altitude <- c(rmvnorm(1, sigma = cov_altitude) * diff(alt_range)/4 + mean(alt_range))
  
  #MOUNTAIN/TERRAIN EFFECT ON g0 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mountain <- ifelse(session_info$terrain == "mountain", beta6, 0)
  
  #SESSION WEATHER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(session_info$weather == "snow") weather <- 1 * beta5
  else if(session_info$weather == "rain") weather <- 2 * beta5
  else if(session_info$weather == "overcast") weather <- 3 * beta5
  else weather <- 4 * beta5
  g0 <-  plogis(qlogis(g0_base) + mountain + weather)

  #ANIMAL LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Animal locations saved here for future plotting
  animal_locations <- data.frame(matrix(ncol = 2))
  names(animal_locations) <- c("x", "y")
  
  #CALCULATE DISTANCES TO NEAREST VILLAGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Find distance from all mask locations to nearest village, if any exist
  distances_to_nearest_village <- numeric()
  if(!is.null(village_locations)) {
    nearest_village_distance <- sqrt((village_locations$x - 
                                            mask_locations)^2+(village_locations$y - mask_locations)^2) |> 
      rowwise() |> 
      mutate(min_val = min(c_across(everything()), na.rm = TRUE)) |> 
      pull(min_val)
  } else nearest_village_distance <- 0

  #CALCULATE DENSITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Get the densities inside all mask cells
  if(!is.null(village_locations)) {
    D <-  exp(beta0 + beta1*mask_locations[,1] + 
                beta2*mask_locations[,2] + 
                beta3*sim.forest +
                beta4*nearest_village_distance +
                beta7*sim.protected +
                beta8*mask_locations_with_alt$altitude +
                beta9*session_info$time)/10000
  } else {
    D <-  exp(beta0 + beta1*mask_locations[,1] + 
                beta2*mask_locations[,2] + 
                beta3*sim.forest +
                beta7*sim.protected +
                beta8*mask_locations_with_alt$altitude +
                beta9*session_info$time)/10000
  }
  
  
  #FIND THE ANIMALS WITHIN THE MASK CELLS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #get the expected number of animals within all the cells
  animals_in_cells <- rpois(length(D), mask_cell_area * D)
  
  #ignore mask cells where there are no animals
  zero_animal_cells <- which(animals_in_cells == 0)
  #edge case for if every single cell has an animal in it
  if(length(zero_animal_cells) == 0) {
    populated_cells <- mask_locations
  } else {
    populated_cells <- mask_locations[-zero_animal_cells,]
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
  distances <- rdist(animal_coords, detector_locations)
  
  #hence find their detection probabilities
  probabilities <- g0 * exp(-distances^2/(2*sigma^2)) 
  random_capture_hist <- matrix(rbinom(length(probabilities), 1, probabilities), nrow(probabilities), ncol(probabilities)) 
  
  #Get the coordinates of the animals that WERE detected
  detected_animal_coords <- animal_coords[which(rowSums(random_capture_hist) > 0),]
  
  #We don't detect the animals with no captures in their histories, so delete those
  random_capture_hist <- subset(random_capture_hist, rowSums(random_capture_hist) > 0)

  #Use format_capture_histories to create long format capture history matrix
  #First need to add row number to get animal_id
  random_capture_hist_alt <- cbind(random_capture_hist, 1:nrow(random_capture_hist))
  long_capt_hist <- do.call(rbind, apply(random_capture_hist_alt, 1, format_capture_histories, session_info$session))
  for(i in 1:ncol(long_capt_hist)) long_capt_hist[,i] <- as.numeric(long_capt_hist[,i])
  
  
  #BEARINGS OF ANIMALS FROM DETECTORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #find TRUE bearings of all detected animals to the detectors that actually detected them
  #assign detector ID to each detector location
  detector_locations <- as.data.frame(detector_locations)
  names(detector_locations) <- c("detector_x", "detector_y")
  detector_locations$detector_ID <- 1:nrow(detector_locations)
  
  #assign animal IDs
  detected_animal_coords <- as.data.frame(detected_animal_coords)
  detected_animal_coords$ID <- 1:nrow(detected_animal_coords)
  all_info_long_capt_hist <- long_capt_hist %>%
    left_join(detected_animal_coords) %>%
    left_join(detector_locations, by = c("trap" = "detector_ID"))
  true_bearings <- numeric()
  observed_bearings <- numeric()

  #if the session is mountainous, the cp can expect to change
  is_mountain <- ifelse(session_info$terrain == "mountain", 1, 0)
  cp_adjusted <- exp(cp_beta0 + cp_beta1*is_mountain)
  
  #TODO vectorise this?
  for(i in 1:nrow(all_info_long_capt_hist)) {
    to_coords <- all_info_long_capt_hist[i,5:6]
    from_coords <- all_info_long_capt_hist[i,7:8]
    true_bearing <- find_bearings(from_coords, to_coords)
    true_bearings[i] <- true_bearing[[1]]
    observed_bearings[i] <- rvm(1, true_bearing[[1]], cp_adjusted) #bearings more accurate in non-mountain - log(cp) = beta0 (use log50) + beta1*mountain (beta1 negative)
  }

  long_capt_hist$bearing <- observed_bearings

  traps <- data.frame("x" = detector_locations[,1], 
                      y = detector_locations[,2])
  
  #and return the resulting capture history as well as the locations of all animals for plotting
  list("binary_capture_history" = random_capture_hist,
       "long_form_capture_history" = long_capt_hist, #add observed bearings column
       "animal_locations" = na.omit(animal_coords),
       "detected_animal_locations" = detected_animal_coords,
       "forest" = as.character(sim.forest),
       "protected_areas" = as.character(sim.protected),
       "altitude" = mask_locations_with_alt$altitude,
       "traps" = traps,
       "mask" = mask_locations,
       "mask_cell_area" = mask_cell_area,
       "nearest_village_distance" = nearest_village_distance)
  
}
