#' @export
nullclines_2d <- function(s, p, m, x, y, x_range, y_range, x_grid_step = 500, y_grid_step = 500, zero_nudge = -0.01) {

  # Need to write checks for x_range, y_range and grid_step (grid_step can't be zero!)
  # Do not allow grid_step to go below 0.001, as it becomes memory intensive.
  # Plot with:
  # foo %>%
  #   ggplot(aes(x = y, y = x, col = nullcline)) +
  #   geom_path()

  x_nm <- names(s)[x]
  y_nm <- names(s)[y]

  # Nudge range a tiny to the left if minima are zero.
  # REFACTOR THIS PART: make it simpler.
  x_range[1] <- ifelse(dplyr::near(x_range[1], 0, tol = 1e-4), zero_nudge, x_range[1])
  x_range[2] <- ifelse(dplyr::near(x_range[1], 0, tol = 1e-4), zero_nudge, x_range[2])
  y_range[1] <- ifelse(dplyr::near(y_range[1], 0, tol = 1e-4), zero_nudge, y_range[1])
  y_range[2] <- ifelse(dplyr::near(y_range[1], 0, tol = 1e-4), zero_nudge, y_range[2])

  x_grid_ <- seq(x_range[1], x_range[2], length.out = x_grid_step)
  y_grid_ <- seq(y_range[1], y_range[2], length.out = y_grid_step)

  nx <- length(x_grid_)
  ny <- length(y_grid_)
  n <- nx * ny

  s_ <- lapply(s, rep, n)
  p_ <- lapply(p, rep, n)

  s_[[x]] <- rep(x_grid_, each = ny)
  s_[[y]] <- rep(y_grid_, times = nx)

  dv <- m(0, s_, p_)[[1]]
  nvar <- length(s)
  dim(dv) <- c(ny, nx, nvar)

  isolines1 <-
    tibble::as_tibble(isoband::isolines(x_grid_, y_grid_, dv[, , 1], 0)[[1]]) %>%
    tibble::add_column(nullcline = x_nm, .before = 1)

  isolines2 <-
    tibble::as_tibble(isoband::isolines(x_grid_, y_grid_, dv[, , 2], 0)[[1]]) %>%
    tibble::add_column(nullcline = y_nm, .before = 1)

  x_decimal_places <- max(round(log10(x_grid_step)) + 1, 1)
  y_decimal_places <- max(round(log10(y_grid_step)) + 1, 1)

  isolines <- dplyr::bind_rows(isolines1, isolines2) %>%
    dplyr::mutate(x = round(x, x_decimal_places),
                  y = round(y, y_decimal_places)) %>%
    dplyr::select(-id)

  return(isolines)
}
