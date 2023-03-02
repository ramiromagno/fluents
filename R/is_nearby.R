is_nearby1 <- function(x0, x1, k = 0.1, epsilon = 1e-9) {
  arclength1(x0, x1) < k * (epsilon + norm1(x0))
}

is_nearby2 <- function(x0, x1, k = 0.1, epsilon = 1e-9) {
  arclength2(x0, x1) < k * (epsilon + norm2(x0))
}
