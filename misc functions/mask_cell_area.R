mask_cell_area <- function(mask) {
  sqrt((max(range(mask[,1])) - min(range(mask[,1])))^2) *
  sqrt((max(range(mask[,2])) - min(range(mask[,2])))^2)
}