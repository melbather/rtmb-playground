#Function to generate information/covariates about a given number of sessions
generate_session_information <- function(num_of_sessions, conditions1, conditions2, time_range,
                                         x_dimension, y_dimension, min_space = 50, space_overlap = FALSE) {
  #first set of conditions, e.g. weather
  rand_conditions1 <- sample.int(length(conditions1), num_of_sessions, replace = TRUE)
  #second set of conditions, e.g. mountain/non-mountain
  rand_conditions2 <- sample.int(length(conditions2), num_of_sessions, replace = TRUE)
  #generate time (only in years for now)
  time <- sample(min(time_range):max(time_range), num_of_sessions)
  session_locations <- generate_session_locations(num_of_sessions, x_dimension, 
                                                  y_dimension, min_space, space_overlap)
  sessions <- data.frame(cbind(1:num_of_sessions, 
                               conditions1[rand_conditions1], 
                               conditions2[rand_conditions2]), 
                         time, session_locations)
  names(sessions) <- c("session", "weather", "terrain", "time", 
                       "x_start", "x_end", "y_start", "y_end")
  sessions
}
