#' Integrate a system of differential equations
#'
#' This function performs numerical integration of a system of Ordinary
#' Differential Equations (ODE) or of Delay Differential Equations (DDE).
#'
#' @param s Starting state vector.
#' @param p Parameter vector.
#' @param m Model, i.e. a function that calculates the values of the derivatives
#'   of the system of differential equations.
#' @param t Time points of the returned solution.
#' @param solver_type A string, either `"ode"`, if an ODE system, or `"dede"`,
#'   if a DDE system.
#' @param ... Extra arguments that are passed on to either [`deSolve::ode()`] or
#'   [`deSolve::dede()`].
#'
#' @return A [tibble][tibble::tibble-package] whose first column is time and
#'   remaining columns are state variables as provided in `s`. The time column
#'   is named `t` unless a state variable also named `t` exists; in that case it
#'   gets renamed to `.t`.
#'
#' @examples
#' # Integrate the Lotka-Volterra model
#' m <- function(t, state, parms) {
#' with(as.list(c(state, parms)), {
#'
#'   dR <- r*R*(1 - R/K) - a*R*N
#'   dN <- c*a*R*N - delta*N
#'
#'   return(list(c(dR, dN)))
#' })
#' }
#'
#' # `p` is a named vector of parameters
#' p <- c(r = 1, K = 1, a = 1, c = 1, delta = 0.5)
#'
#' # `s` is the initial state vector
#' s <- c(R = 0.6, N = 0.4)
#'
#' # `t` is the time sequence
#' t <- seq(0, 30, by = 1)
#'
#' solve(s = s, p = p, m = m, t = t)
#'
#' @export
solve <- function(s, p, m, t, solver_type = c('ode', 'dede'), ...) {

  # Is the type of solver specified one of: 'ode' or 'dede'?
  solver_type <- match.arg(solver_type)

  # Pick the corresponding solver function from deSolve.
  solver <-
    switch(solver_type,
           ode = deSolve::ode,
           dede = deSolve::dede)

  # Integrate the model `m` starting at the state `s` using parameters `p` and
  # save the solution at the time points `t`.
  solution <- solver(y = s, times = t, func = m, parms = p, ...)

  # Rename the time column to `t`, or `.t` to avoid clashes with state variables
  # that might be named `t` or `time`.
  solution <- rename_time_col(solution)

  # Convert to tibble
  tbl <- tibble::as_tibble(solution) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.numeric))

  return(tbl)
}

rename_time_col <- function(solution) {
  if ("t" %in% colnames(solution)) {
    colnames(solution)[1] <- ".t"
  } else {
    colnames(solution)[1] <- "t"
  }
  solution
}
