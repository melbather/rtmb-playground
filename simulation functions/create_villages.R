#Function to create village locations within specified area
create_villages <- function(number, x_range, y_range) {
  locations <- data.frame(matrix(nrow = number, ncol = 2))
  names(locations) <- c("x", "y")
  locations$x <- runif(number, min = min(x_range), max = max(x_range))
  locations$y <- runif(number, min = min(y_range), max = max(y_range))
  locations
}
