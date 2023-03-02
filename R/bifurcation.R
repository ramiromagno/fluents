# continue <- function(s0, s_i, p0, p_i, dp, m, s_range = c(0, 1.1), p_range = c(0, 1), positive = TRUE) {
#
#   # n_p: Number of parameters.
#   n_p <- length(p0)
#
#   # n_s: Number of state variables.
#   n_s <- length(s0)
#
#   # p: List of 3 bifurcation parameter vectors: previous and current.
#   p <- list(previous = p0, current = p0)
#
#   # s: List of 3 state vectors: previous, current and coming.
#   # NB: `next` is a reserved word in R so `coming` is used instead.
#   s <- list(previous = s0, current = s0, coming = rep(NA_real_, n_s))
#
#   # nok: Number of "ok" steps.
#   nok <- 0L
#
#   while(between(p$current[p_i], p_range) && between(s$current[s_i], s_range)) {
#
#     # Update current parameter vector from the previous.
#     p$current[p_i] <- p$previous[p_i] + dp
#
#     # Find steady-state.
#     q <- rootSolve::steady(y = s$current, parms = p$current, fun = m, positive = positive)
#
#     s$coming <- q$y
#
#   if(is_steady(q) && is_nearby2(x0 = s$current, x1 = s$coming)) {
#
#     jac <- jacobian.full(y = s$coming, fun = m, parms = p$current)
#
#
#   }
#
#
#   }
# }

new_parameter_value <- function(p, step = 0.01, log_scale = FALSE) {
  ifelse(log_scale, p*(1+step), p + step)
}

#' bifurcation
#' @export
bifurcation <-
  function(s0,
           p0,
           m,
           p_i = 1L,
           s_i = 1L,
           step = 0.01,
           p_range = c(0, 1),
           s_range = c(0, 1.1),
           log_scale_axis = '',
           positive = FALSE,
           steady_warnings = FALSE,
           ...) {

    if (!(rlang::is_integerish(p_i) || is.character(p_i)))
      stop('`p_i` must be an index or a name for `p`')

    if (rlang::is_integerish(p_i)) {
      if (!(p_i %in% seq_along(p0)))
        stop('`p_i` is out of bounds')
    }

    if (is.character(p_i)) {
      if (!(p_i %in% names(p0)))
        stop('`p_i` is not a name of `p0`')
    }

    if (!(rlang::is_integerish(s_i) || is.character(s_i)))
      stop('`s_i` must be an index or a name for `s0`')

    if (rlang::is_integerish(s_i)) {
      if (!(s_i %in% seq_along(s0)))
        stop('`s_i` is out of bounds')
    }

    if (is.character(s_i)) {
      if (!(s_i %in% names(s0)))
        stop('`s_i` is not a name of `s0`')
    }

  # Check if initial parameter value is within range
  if(!between(p0[p_i], p_range)) stop('Initial parameter value not within range defined by `p_range`')

  # Determine the steady-state, given the initial guess `s0` and the initial parameter vector `p0`.
  q0 <- steady(y = s0, fun = m, parms = p0, positive = positive, .warnings = steady_warnings, ...)

  if(!is_steady(q0)) stop('No convergence: start closer to a steady state!')

  # If everything is all right thus far, then move on to the actual continuation algorithm
  cat("Starting at",names(p0[p_i]),"=",p0[p_i],"with:\n")
  print(q0$y)

  # Check that the initial equilibrium is within range.
  if(!between(q0$y[s_i], s_range)) stop('State variable not within range defined by `s_range`')

  # Determine the stability of the initial steady-state.
  stb0 <- sign_to_stability(sign_of_dominant_eigenvalue(s = q0$y, p = p0, m = m))

  if (identical(log_scale_axis, 'x')) actualStep <- step
  else actualStep <- step*p_range[2]

  path_right <- continue(
    s0 = q0$y,
    s_i = s_i,
    p0 = p0,
    p_i = p_i,
    stb0 = stb0,
    step0 = actualStep,
    s_range = s_range,
    p_range = p_range,
    step_range = c(actualStep / 100, actualStep),
    m = m,
    log_scale_axis = log_scale_axis,
    positive = positive,
    steady_warnings = steady_warnings
  )

  path_left <- continue(
    s0 = q0$y,
    s_i = s_i,
    p0 = p0,
    p_i = p_i,
    stb0 = stb0,
    step0 = -actualStep,
    s_range = s_range,
    p_range = p_range,
    step_range = c(actualStep / 100, actualStep),
    m = m,
    log_scale_axis = log_scale_axis,
    positive = positive,
    steady_warnings = steady_warnings
  )

  path_left %>%
    dplyr::mutate(.i = -.i) %>%
    dplyr::arrange(.i) %>%
    dplyr::bind_rows(path_right) %>%
    dplyr::mutate(.segment = as.integer(cumsum(.stability != dplyr::lag(.stability, default = .stability[1])) + 1)) %>%
    dplyr::relocate('.segment', .before = .path)
}


continue <- function(s0, s_i, p0, p_i, stb0, step0, s_range, p_range, step_range, m, log_scale_axis, positive, steady_warnings = FALSE, ...) {

  # Initial setup of the continuation variables
  ## Parameter vectors p:
  p_current <- p0
  p_tentative <- p0
  p_tentative[p_i] <- NA_real_

  ## State vectors s:
  s_previous <- s0
  s_current <- s0
  s_tentative <- rep(NA_real_, length(s_current))

  ## Stability stb
  stb_current <- stb0
  stb_tentative <- NA_real_

  ## Step
  step <- step0
  step_minimum <- step_range[1]
  step_maximum <- step_range[2]

  # Number of okay steps
  nok <- 0

  # Environment to keep the bifurcation tibble and iterator i.
  bif_env <- new.env()

  # Create the bifurcation tibble
  add_new_bif_tbl(e = bif_env, n = min(diff(p_range) / step_minimum, 1000L))
  push_bif_point(bif_env, parameter = p_current[p_i], state = s_current[s_i], stability = stb_current, p = p_current, s = s_current)

  while (between(p_current[p_i], p_range) && between(s_current[s_i], s_range)) {

    p_tentative[p_i] <- new_parameter_value(p = p_current[p_i], step = step, log_scale = identical(log_scale_axis, 'x'))
    q <- steady(y = s_current, fun = m, parms = p_tentative, positive = positive, .warnings = steady_warnings, ...)
    s_tentative <- q$y  # should be steady state and closeby

    if (is_steady(q) && is_nearby1(s_current, s_tentative)) {

      stb_tentative <- sign_to_stability(sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = m))

      if (stb_tentative != stb_current) {
        cat("Bifurcation at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
        s_middle <- (s_current + s_tentative) / 2
        p_middle <- (p_current + p_tentative) / 2
        push_bif_point(bif_env, parameter = p_middle[p_i], state = s_middle[s_i], stability = 'neutral', p = p_middle, s = s_middle)
      }

      s_previous <- s_current
      s_current <- s_tentative
      stb_current <- stb_tentative
      p_current[p_i] <- p_tentative[p_i]

      if (nok > 10 & abs(step) < step_maximum) step <- sign(step)*min(2*abs(step), step_maximum)
      nok <- nok + 1

      push_bif_point(bif_env, parameter = p_current[p_i], state = s_current[s_i], stability = stb_current, p = p_current, s = s_current)

    } else {
      nok <- 0
      if (abs(step) > step_minimum) step <- step / 2
      else { # Go back one step, over-predict, call steady, and turn.

        p_tentative[p_i] <- p_current[p_i]
        predState <- s_current + 5*(s_current-s_previous)
        q <- steady(y = predState, fun = m, parms = p_tentative, positive = positive, .warnings = steady_warnings, ...)

        s_tentative <- q$y  # should be steady state and not the same
        if (is_steady(q) && is_afar1(s_current, s_tentative)) {

          cat("Turning point point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
          stb_tentative <- sign_to_stability(sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = m))

          s_middle_i <- (s_current[s_i] + s_tentative[s_i]) / 2
          s_middle <- (s_current + s_tentative) / 2
          push_bif_point(bif_env, parameter = p_current[p_i], state = s_middle_i, stability = 'neutral', p = p_current, s = s_middle)

          step <- -step
          s_previous <- s_current
          s_current <- s_tentative
          stb_current <- stb_tentative
          p_current[p_i] <- p_tentative[p_i]

          # Logging
          push_bif_point(bif_env, parameter = p_current[p_i], state = s_current[s_i], stability = stb_current, p = p_current, s = s_current)

        } else {
          cat("Final point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
          cat("If this looks wrong try changing the step size\n")
          break
        }
      }
    }
  }

  # Prepend a column with the path direction,
  # and remove empty rows, and return value.
  bif_env$tbl %>%
    tibble::add_column(.path = dplyr::if_else(step0 > 0, 'right', 'left'), .after = '.stability') %>%
    tidyr::drop_na()
}

new_bif_tbl <- function(n) {

  tibble::tibble(
    .i = rep(NA_integer_, n),
    .par = NA_real_,
    .var = NA_real_,
    .stability = NA_character_,
    .p = vector("list", n),
    .s = vector("list", n)
  )
}

add_new_bif_tbl <- function(e, n) {

  e$i <- 0
  e$tbl <- new_bif_tbl(n)
}

push_bif_point <- function(e, parameter, state, stability, p, s, growth = 1000L) {

  # Increment the iteration value i by 1.
  e$i <- e$i + 1

  if(e$i > nrow(e$tbl)) {
    tbl2 <- new_bif_tbl(growth)
    e$tbl <- dplyr::bind_rows(e$tbl, tbl2)
  }

  e$tbl[e$i, '.i'] <- e$i
  e$tbl[e$i, '.par'] <- parameter
  e$tbl[e$i, '.var'] <- state
  e$tbl[e$i, '.stability'] <- stability
  e$tbl[e$i, '.p'][[1]] <- list(p)
  e$tbl[e$i, '.s'][[1]] <- list(s)

}


