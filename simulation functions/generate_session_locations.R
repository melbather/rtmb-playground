generate_session_locations <- function(num_of_sessions, x_dimension, 
                                       y_dimension, min_space, space_overlap = FALSE) {
  
  if(!space_overlap) {
    #min_space = minimum amount of space that needs to separate each session
    x_range <- (x_dimension + min_space) * num_of_sessions * 3 
    y_range <- (y_dimension + min_space) * num_of_sessions * 3
    
    #partition x and y ranges into squares where sessions can lie without overlapping
    x_lims <- seq(0, x_range, length.out = num_of_sessions + 1)
    y_lims <- seq(0, y_range, length.out = num_of_sessions + 1)
    
    #create place to store output coords
    session_coords <- data.frame(x_start = numeric(0),
                                 x_end = numeric(0),
                                 y_start = numeric(0),
                                 y_end = numeric(0))
    
    #create sessions inside the partitioned areas created above
    for(i in 1:num_of_sessions) {
      x_start <- sample(x_lims[i]:x_lims[i+1]-1, 1)
      y_start <- sample(y_lims[i]:y_lims[i+1]-1, 1)
      x_end <- x_start + x_dimension
      y_end <- y_start + y_dimension
      session_coords[i,] <- c(x_start, x_end, y_start, y_end)
    }
  } else {
    x_range <- x_dimension * num_of_sessions
    y_range <- y_dimension * num_of_sessions 
    
    #create place to store output coords
    session_coords <- data.frame(x_start = numeric(0),
                                 x_end = numeric(0),
                                 y_start = numeric(0),
                                 y_end = numeric(0))
    
    #create sessions inside the partitioned areas created above
    for(i in 1:num_of_sessions) {
      x_start <- sample(0:x_range, 1)
      y_start <- sample(0:y_range, 1)
      x_end <- x_start + x_dimension
      y_end <- y_start + y_dimension
      session_coords[i,] <- c(x_start, x_end, y_start, y_end)
    }
  }

  session_coords
}
