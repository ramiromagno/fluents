# This avoids the warnings from stode (and the like), such as:
# "In stode(y, times, func, parms = parms, ...) : steady-state not reached"
steady <- function(.warnings = FALSE, ...) {
  ss <-
    if (.warnings) {
      rootSolve::steady(...)
    } else {
      # The `supress_output()` call is buggy.
      suppressWarnings(rootSolve::steady(...))
      # suppressMessages(rootSolve::steady(...))
    }

  return(ss)
}

is_steady <- function(ss) {
  !is.null(attr(ss, "steady")) && attr(ss, "steady")
}

is_stable <- function(eigenvalues) {
  all(Re(eigenvalues) < 0)
}

sign_of_dominant_eigenvalue <- function(s, p, m) {

  jacobian_matrix <- rootSolve::jacobian.full(y = s, fun = m, parms = p)
  eigenvalues <- eigen(jacobian_matrix)$values
  dominant_eigenvalue <- max(Re(eigenvalues))
  sign <- sign(dominant_eigenvalue)

  return(sign)
}

sign_to_stability <- function(sign) {

  stb <- rep(NA_character_, n = length(sign))

  stb[sign == 1] <- 'unstable'
  stb[sign == -1] <- 'stable'
  stb[sign == 0] <- 'neutral'

  return(stb)
}

find_steady_state_ <- function(s0, p, m, rounding_digits = 5, ...) {

  # Determine steady-state (ss)
  ss <- steady(y = s0, fun = m, parms = p, ...)

  # s0: state initial guess as provided by the user.
  # s0_lst: a list alike s0 but whose names have been prefixed with `..`.
  s0_lst <- setNames(as.list(s0), paste0('..', names(s0)))

  # Was `rootSolve::steady` successful in finding a steady-state?
  if(is_steady(ss)) {
    # s: steady-state vector as determined by `rootSolve::steady()`
    s <- round(ss$y, digits = rounding_digits)
    s_lst <- as.list(s)

    # Determine stability.
    jac <- rootSolve::jacobian.full(y = s, fun = m, parms = p)
    jac <- round(jac, digits = rounding_digits)
    eigen_ <- eigen(jac)

    # If the real part of all eigenvalues is less than 0, then we have a
    # stable point, otherwise, it is unstable.
    stability <- ifelse(is_stable(eigen_$values), 'stable', 'unstable')

    stbl_tbl <- tibble::tibble_row(
      stability = stability,
      jacobian = list(jac),
      eigenvalues = list(eigen_$values),
      eigenvectors = list(eigen_$vectors),
      is_steady_state = TRUE
    )

    tbl <- dplyr::bind_cols(tibble::as_tibble(c(s0_lst, s_lst)), stbl_tbl)

    return(tbl)
  }

  # If rootSolve::steady did not find a steady-state, then return NAs.
  if (!is_steady(ss)) {

    # The steady-state not found, i.e., a vector of NAs.
    s <- setNames(rep(NA_real_, length(s0)), nm = names(s0))
    s_lst <- as.list(s)

    stbl_tbl <- tibble::tibble_row(
      stability = NA_character_,
      jacobian = list(NULL),
      eigenvalues = list(NULL),
      eigenvectors = list(NULL),
      is_steady_state = FALSE
    )

    tbl <- dplyr::bind_cols(tibble::as_tibble(c(s0_lst, s_lst)), stbl_tbl)

    return(tbl)
  }

}

#' @export
find_steady_state <- function(s0, p, m, rounding_digits = 5, distinct = FALSE, ...) {

  if(is.atomic(s0)) s0 <- tibble::as_tibble(as.list(s0))
  # Else, `s0` is expected to be a data frame.
  nm <- colnames(s0)

  tbl <-
    apply(
      X = s0,
      MARGIN = 1,
      FUN = find_steady_state_,
      p = p,
      m = m,
      rounding_digits = rounding_digits,
      ...
    ) %>%
    dplyr::bind_rows()

  if(distinct) return(dplyr::distinct(tbl, dplyr::across(dplyr::all_of(nm)), .keep_all = TRUE))
  else return(tbl)
}

#' @importFrom rlang .data
#' @export
steady_states <- function(s0, p, m, rounding_digits = 5, distinct = TRUE) {
  find_steady_state(
    s0 = s0,
    p = p,
    m = m,
    rounding_digits,
    distinct = distinct
  ) %>%
    dplyr::filter(.data$is_steady_state) %>%
    dplyr::select(-"is_steady_state")
}
