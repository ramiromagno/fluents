snap <- function(x, min, max, step) {
  grid <- seq(min, max, by = step)
  grid[findInterval(x, c(-Inf, grid + step / 2, Inf))]
}
