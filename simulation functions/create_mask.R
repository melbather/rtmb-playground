#Function to generate an ellipse-shaped mask
create_mask <- function(x_width, y_width, num_rows, num_cols) {
  x_spacing <- 2 * x_width/(num_cols - 1)
  y_spacing <- 2 * y_width/(num_rows - 1)
  
  x_coords <- seq(-x_width, x_width, by = x_spacing)
  y_coords <- seq(-y_width, y_width, by = y_spacing)
  
  points <- expand.grid(x = x_coords, y = y_coords)
  
  inside_ellipse <- (points$x^2/x_width^2 + points$y^2/y_width^2) <= 1
  ellipse_points <- points[inside_ellipse, ]
  ellipse_points
}
