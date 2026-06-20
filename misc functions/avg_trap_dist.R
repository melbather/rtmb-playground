avg_trap_dist <- function(traps) {
  distances <- unlist(lapply(traps, function(x) {
    sqrt(rowSums((x[-1, , drop = FALSE] - x[-nrow(x), , drop = FALSE])^2))
  }))

  mean(distances)
}