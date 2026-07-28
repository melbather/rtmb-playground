avg_trap_dist <- function(traps) {

  # the problem with this approach is that it doesn't consider a grid
  # need to figure out a way that the function accounts for the grid-ness
  # i.e. it works fine if all the coords are in a line
  # maybe do something like adding a column called "is_adj" for if the x or y dists == 0

  # for simplicity, just use the first element in the traps list
  coord_diffs <- diff(traps[[1]])
  distances <- sqrt(rowSums(coord_diffs^2))

  mean(distances)
}