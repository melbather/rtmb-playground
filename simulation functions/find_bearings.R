#this function takes in two coordinate pairs and finds the true bearing of the second pair from the first pair
#it deals with four cases based on the relative positioning of the "to" coordinates from the "from" coordinates
find_bearings <- function(from_coords, to_coords) {
  x1 <- from_coords[1]
  y1 <- from_coords[2]
  x2 <- to_coords[1]
  y2 <- to_coords[2]
  #case 1: the bearing is in the "first quadrant"
  if(y2 >= y1 & x2 >= x1) {
    atan(abs(x2-x1)/abs(y2-y1))
  } else if(y2 <= y1 & x2 >= x1) {
    #case 2: the bearing is in the "second quadrant"
    atan(abs(y2-y1)/abs(x2-x1)) + pi/2
  } else if(y2 <= y1 & x2 <= x1) {
    #case 3: the bearing is in the "third quadrant"
    atan(abs(x2-x1)/abs(y2-y1)) + pi
  } else {
    #case 4: the bearing is in the "fourth quadrant"
    2*pi - atan(abs(x2-x1)/abs(y2-y1))
  }
}