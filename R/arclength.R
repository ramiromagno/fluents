# arclength using 1-norm
arclength1 <- function(x0, x1) {
  norm1(x1 - x0)
}

# arclength using 2-norm (euclidean distance)
arclength2 <- function(x0, x1) {
  norm2(x1 - x0)
}
