#Function to change binary capture histories into format that can be used by acre
#This function takes in a single line of a binary capture history and turns it into long format data
#It only does a single line, so needs to be used with an apply function to convert an entire matrix
#Before using this function an animal ID needs to be assigned to the animal. 
#The animal ID should just be the row number of the animal in the binary capture history matrix
#This function is for a single session
format_capture_histories <- function(binary_capt_hist, session_id) {
  #Animal ID should be in the last column. Save and remove from capt hist data
  animal_id <- as.numeric(binary_capt_hist[length(binary_capt_hist)])
  binary_capt_hist <- as.numeric(binary_capt_hist[-length(binary_capt_hist)])
  #Find where the animal was actually detected
  detection_locations <- as.numeric(which(binary_capt_hist == 1))
  #Add to df for output
  animal_ids <- as.numeric(rep(animal_id, length(detection_locations)))
  df <- data.frame(cbind(session_id, animal_ids, 1, detection_locations))
  names(df) <- c("session", "ID", "occasion", "trap")
  df
}