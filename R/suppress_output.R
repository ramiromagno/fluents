suppress_output <- function(...) {
  capture.output(..., file = nullfile(), type = "message")
}
