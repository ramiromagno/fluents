#' # continue <- function(s0, s_i, p0, p_i, dp, m, s_range = c(0, 1.1), p_range = c(0, 1), positive = TRUE) {
#' #
#' #   # n_p: Number of parameters.
#' #   n_p <- length(p0)
#' #
#' #   # n_s: Number of state variables.
#' #   n_s <- length(s0)
#' #
#' #   # p: List of 3 bifurcation parameter vectors: previous and current.
#' #   p <- list(previous = p0, current = p0)
#' #
#' #   # s: List of 3 state vectors: previous, current and coming.
#' #   # NB: `next` is a reserved word in R so `coming` is used instead.
#' #   s <- list(previous = s0, current = s0, coming = rep(NA_real_, n_s))
#' #
#' #   # nok: Number of "ok" steps.
#' #   nok <- 0L
#' #
#' #   while(between(p$current[p_i], p_range) && between(s$current[s_i], s_range)) {
#' #
#' #     # Update current parameter vector from the previous.
#' #     p$current[p_i] <- p$previous[p_i] + dp
#' #
#' #     # Find steady-state.
#' #     q <- rootSolve::steady(y = s$current, parms = p$current, fun = m, positive = positive)
#' #
#' #     s$coming <- q$y
#' #
#' #   if(is_steady(q) && is_nearby2(x0 = s$current, x1 = s$coming)) {
#' #
#' #     jac <- jacobian.full(y = s$coming, fun = m, parms = p$current)
#' #
#' #
#' #   }
#' #
#' #
#' #   }
#' # }
#'
#' new_parameter_value <- function(p, step = 0.01, log_scale = FALSE) {
#'   ifelse(log_scale, p*(1+step), p + step)
#' }
#'
#'
#' #' continue
#' #' @export
#' continue <-
#'   function(state,
#'            parms,
#'            odes = model,
#'            p_i = 1L,
#'            s_i = 1L,
#'            step = 0.01,
#'            p_range = c(0, 1),
#'            s_range = c(0, 1.1),
#'            log_scale_axis = '',
#'            # time = 0,
#'            positive = FALSE,
#'            add = FALSE,
#'            ...) {
#'
#'
#'     if (!(rlang::is_integerish(p_i) || is.character(p_i)))
#'       stop('`p_i` must be an index or a name for `p`')
#'
#'     if (rlang::is_integerish(p_i)) {
#'       if (!(p_i %in% seq_along(parms)))
#'         stop('`p_i` is out of bounds')
#'     }
#'
#'     if (is.character(p_i)) {
#'       if (!(p_i %in% names(parms)))
#'         stop('`p_i` is not a name of `parms`')
#'     }
#'
#'     if (!(rlang::is_integerish(s_i) || is.character(s_i)))
#'       stop('`s_i` must be an index or a name for `state`')
#'
#'     if (rlang::is_integerish(s_i)) {
#'       if (!(s_i %in% seq_along(state)))
#'         stop('`s_i` is out of bounds')
#'     }
#'
#'     if (is.character(s_i)) {
#'       if (!(s_i %in% names(state)))
#'         stop('`s_i` is not a name of `state`')
#'     }
#'
#'     # Set plot parameters
#'     clrs <- c("red","black","blue")
#'     lwds <- c(2,1,1)
#'
#'     # Check if initial parameter value is within range
#'     if(!between(parms[p_i], p_range)) stop('Initial parameter value not within range defined by `p_range`')
#'
#'     # Determine the steady-state, given the initial guess `state` and the initial parameter vector `parms`.
#'     q0 <- rootSolve::steady(y = state, fun = odes, parms = parms, positive = positive, ...)
#'
#'     if (is_steady(q0)) {
#'       cat("Starting at",names(parms[p_i]),"=",parms[p_i],"with:\n")
#'       print(q0$y)
#'
#'       # Check that the initial equilibrium is within range.
#'       if(!between(q0$y[s_i], s_range)) stop('State variable not within range defined by `s_range`')
#'
#'       # Setup plot canvas
#'       if(!add) {
#'         plot(1, 1, type = 'n', xlim = p_range, ylim = s_range, xlab = names(parms[p_i]), ylab = names(state)[s_i], log = log_scale_axis, font.main = 1, font.sub = 1, ...)
#'       }
#'
#'       # orgWarn <- getOption("warn")
#'       # options(warn = -1)
#'       # Determine the stability of the initial steady-state
#'       dom <- sign_of_dominant_eigenvalue(s = q0$y, p = parms, m = odes)
#'
#'       if (identical(log_scale_axis, 'x')) actualStep <- step
#'       else actualStep <- step*p_range[2]
#'
#'       # FUN(
#'       #   p = parms,
#'       #   p_i = p_i,
#'       #   s_i = s_i,
#'       #   lastState = q0$y,
#'       #   lastDom = dom,
#'       #   step = actualStep,
#'       #   p_range = p_range,
#'       #   s_range = s_range,
#'       #   odes = odes,
#'       #   log_scale_axis = log_scale_axis,
#'       #   positive = positive
#'       # )
#'       # FUN(
#'       #   p = parms,
#'       #   p_i = p_i,
#'       #   s_i = s_i,
#'       #   lastState = q0$y,
#'       #   lastDom = dom,
#'       #   step = -actualStep,
#'       #   p_range = p_range,
#'       #   s_range = s_range,
#'       #   odes = odes,
#'       #   log_scale_axis = log_scale_axis,
#'       #   positive = positive
#'       # )
#'       path_right <- FUN3(
#'         s0 = q0$y,
#'         s_i = s_i,
#'         p0 = parms,
#'         p_i = p_i,
#'         stb0 = dom,
#'         step0 = actualStep,
#'         s_range = s_range,
#'         p_range = p_range,
#'         step_range = c(actualStep / 100, actualStep),
#'         odes = odes,
#'         log_scale_axis = log_scale_axis,
#'         positive = positive
#'       )
#'
#'       path_left <- FUN3(
#'         s0 = q0$y,
#'         s_i = s_i,
#'         p0 = parms,
#'         p_i = p_i,
#'         stb0 = dom,
#'         step0 = -actualStep,
#'         s_range = s_range,
#'         p_range = p_range,
#'         step_range = c(actualStep / 100, actualStep),
#'         odes = odes,
#'         log_scale_axis = log_scale_axis,
#'         positive = positive
#'       )
#'
#'       tbl_right <- tibble::add_column(tibble::as_tibble(path_right), direction = 'right', .before = 1)
#'       tbl_left <- tibble::add_column(tibble::as_tibble(path_left), direction = 'left', .before = 1)
#'
#'       tbl <- dplyr::bind_rows(dplyr::arrange(tbl_left, dplyr::desc(iteration)), tbl_right)
#'
#'       return(tbl)
#'       # options(warn = orgWarn)
#'     } else cat("No convergence: start closer to a steady state")
#'   }
#'
#' FUN <- function(p, p_i, s_i, lastState,lastDom, step, p_range, s_range, odes, log_scale_axis, positive, ...) {
#'
#'
#'   clrs <- c("red","black","blue")
#'   lwds <- c(2,1,1)
#'
#'   actualStep <- abs(step)
#'
#'   p0 <- p[p_i]
#'   lastP <- p0
#'   preLastState <- lastState
#'   nok <- 0
#'
#'   while (between(lastP, p_range) && between(lastState[s_i], s_range)) {
#'
#'     p[p_i] <- new_parameter_value(p = lastP, step = step, log_scale = identical(log_scale_axis, 'x'))
#'     q <- rootSolve::steady(y = lastState, fun = odes, parms = p, positive = positive, ...)
#'     newState <- q$y  # should be steady state and closeby
#'
#'     if (is_steady(q) && is_nearby1(lastState, newState)) {
#'
#'       dom <- sign_of_dominant_eigenvalue(s = newState, p = p, m = odes)
#'       if (dom != lastDom) cat("Bifurcation at",names(p[p_i]),"=",p[p_i],"\n")
#'
#'       # Plot continuation
#'       if (identical(log_scale_axis, 'x')) lines(c(p[p_i]/(1+step),p[p_i]),c(lastState[s_i],newState[s_i]), col=clrs[dom+2],lwd=lwds[dom+2])
#'       else lines(c(p[p_i]-step,p[p_i]),c(lastState[s_i],newState[s_i]), col=clrs[dom+2],lwd=lwds[dom+2])
#'
#'       # Update continuation variables
#'       preLastState <- lastState
#'       lastState <- newState
#'       lastDom <- dom
#'       lastP <- p[p_i]
#'
#'       if (nok > 10 & abs(step) < actualStep) step <- sign(step)*min(2*abs(step),actualStep)
#'       nok <- nok + 1
#'
#'     } else {
#'       nok <- 0
#'       if (abs(step) > actualStep/100) step <- step/2
#'       else { # Go back one step, overpredict, call steady, and turn
#'
#'         p[p_i] <- lastP
#'         predState <- lastState + 5*(lastState-preLastState)
#'         q <- rootSolve::steady(y = predState, fun = odes, parms = p, positive = positive, ...)
#'
#'         newState <- q$y  # should be steady state and not the same
#'         if (is_steady(q) && is_afar1(lastState, newState)) {
#'
#'           cat("Turning point point at",names(p[p_i]),"=",p[p_i],"\n")
#'           dom <- sign_of_dominant_eigenvalue(s = newState, p = p, m = odes)
#'
#'           middle <- (lastState[s_i]+newState[s_i])/2
#'           lines(c(p[p_i],p[p_i]),c(lastState[s_i],middle), col=clrs[lastDom+2],lwd=lwds[lastDom+2])
#'           lines(c(p[p_i],p[p_i]),c(middle,newState[s_i]), col=clrs[dom+2],lwd=lwds[dom+2])
#'           step <- -step
#'           preLastState <- lastState
#'           lastState <- newState
#'           lastDom <- dom
#'           lastP <- p[p_i]
#'
#'         } else {
#'           cat("Final point at",names(p[p_i]),"=",p[p_i],"\n")
#'           cat("If this looks wrong try changing the step size\n")
#'           break
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' FUN2 <- function(s0, s_i, p0, p_i, stb0, step0, s_range, p_range, step_range, odes, log_scale_axis, positive, ...) {
#'
#'   clrs <- c("red","black","blue")
#'   lwds <- c(2,1,1)
#'
#'   # Initial setup of the continuation variables
#'   ## Parameter vectors p:
#'   p_current <- p0
#'   p_tentative <- p0
#'   p_tentative[p_i] <- NA_real_
#'
#'   ## State vectors s:
#'   s_previous <- s0
#'   s_current <- s0
#'   s_tentative <- rep(NA_real_, length(s_current))
#'
#'   ## Stability stb
#'   stb_current <- stb0
#'   stb_tentative <- NA_real_
#'
#'   ## Step
#'   step <- step0
#'   step_minimum <- step_range[1]
#'   step_maximum <- step_range[2]
#'
#'   # Number of okay steps
#'   nok <- 0
#'
#'   iteration <- 0
#'
#'   # Environment to keep the bifurcation matrix
#'   bif_env <- new.env()
#'
#'   # Create the bifurcation matrix inside the environment `bif_env`
#'   bif_env$m <- new_bif_matrix(n = min(diff(p_range) / step_minimum, 1000L))
#'
#'   # Calls to `add_row` modify m in-place because it does so by using the
#'   # environment `bif_env`, so we run it for its side effect of changing
#'   # bif_env$m, that's why the return value is not saved.
#'   add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'   while (between(p_current[p_i], p_range) && between(s_current[s_i], s_range)) {
#'
#'     p_tentative[p_i] <- new_parameter_value(p = p_current[p_i], step = step, log_scale = identical(log_scale_axis, 'x'))
#'     q <- rootSolve::steady(y = s_current, fun = odes, parms = p_tentative, positive = positive, ...)
#'     s_tentative <- q$y  # should be steady state and closeby
#'
#'     if (is_steady(q) && is_nearby1(s_current, s_tentative)) {
#'
#'       stb_tentative <- sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = odes)
#'       if (stb_tentative != stb_current) cat("Bifurcation at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'
#'       # Plot continuation
#'       if (identical(log_scale_axis, 'x')) lines(c(p_tentative[p_i]/(1+step),p_tentative[p_i]),c(s_current[s_i],s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'       else lines(c(p_tentative[p_i]-step,p_tentative[p_i]),c(s_current[s_i],s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'
#'       # Update continuation variables
#'       s_previous <- s_current
#'       s_current <- s_tentative
#'       stb_current <- stb_tentative
#'       p_current[p_i] <- p_tentative[p_i]
#'
#'       if (nok > 10 & abs(step) < step_maximum) step <- sign(step)*min(2*abs(step), step_maximum)
#'       nok <- nok + 1
#'
#'       # Logging
#'       iteration <- iteration + 1
#'       add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'     } else {
#'       nok <- 0
#'       if (abs(step) > step_minimum) step <- step / 2
#'       else { # Go back one step, overpredict, call steady, and turn
#'
#'         p_tentative[p_i] <- p_current[p_i]
#'         predState <- s_current + 5*(s_current-s_previous)
#'         q <- rootSolve::steady(y = predState, fun = odes, parms = p_tentative, positive = positive, ...)
#'
#'         s_tentative <- q$y  # should be steady state and not the same
#'         if (is_steady(q) && is_afar1(s_current, s_tentative)) {
#'
#'           cat("Turning point point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'           stb_tentative <- sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = odes)
#'
#'           middle <- (s_current[s_i] + s_tentative[s_i]) / 2
#'           lines(c(p_tentative[p_i],p_tentative[p_i]),c(s_current[s_i],middle), col=clrs[stb_current+2],lwd=lwds[stb_current+2])
#'           lines(c(p_tentative[p_i],p_tentative[p_i]),c(middle,s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'           step <- -step
#'           s_previous <- s_current
#'           s_current <- s_tentative
#'           stb_current <- stb_tentative
#'           p_current[p_i] <- p_tentative[p_i]
#'
#'           # Logging
#'           add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'         } else {
#'           cat("Final point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'           cat("If this looks wrong try changing the step size\n")
#'           break
#'         }
#'       }
#'     }
#'   }
#'   # Remove empty rows
#'   return(bif_env$m[rowSums(is.na(bif_env$m)) != ncol(bif_env$m),])
#' }
#'
#' FUN3 <- function(s0, s_i, p0, p_i, stb0, step0, s_range, p_range, step_range, odes, log_scale_axis, positive, ...) {
#'
#'   clrs <- c("red","black","blue")
#'   lwds <- c(2,1,1)
#'
#'   # Initial setup of the continuation variables
#'   ## Parameter vectors p:
#'   p_current <- p0
#'   p_tentative <- p0
#'   p_tentative[p_i] <- NA_real_
#'
#'   ## State vectors s:
#'   s_previous <- s0
#'   s_current <- s0
#'   s_tentative <- rep(NA_real_, length(s_current))
#'
#'   ## Stability stb
#'   stb_current <- stb0
#'   stb_tentative <- NA_real_
#'
#'   ## Step
#'   step <- step0
#'   step_minimum <- step_range[1]
#'   step_maximum <- step_range[2]
#'
#'   # Number of okay steps
#'   nok <- 0
#'
#'   iteration <- 0
#'
#'   # Environment to keep the bifurcation matrix
#'   bif_env <- new.env()
#'
#'   # Create the bifurcation matrix inside the environment `bif_env`
#'   bif_env$m <- new_bif_matrix(n = min(diff(p_range) / step_minimum, 1000L))
#'
#'   # Calls to `add_row` modify m in-place because it does so by using the
#'   # environment `bif_env`, so we run it for its side effect of changing
#'   # bif_env$m, that's why the return value is not saved.
#'   add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'   while (between(p_current[p_i], p_range) && between(s_current[s_i], s_range)) {
#'
#'     p_tentative[p_i] <- new_parameter_value(p = p_current[p_i], step = step, log_scale = identical(log_scale_axis, 'x'))
#'     q <- rootSolve::steady(y = s_current, fun = odes, parms = p_tentative, positive = positive, ...)
#'     s_tentative <- q$y  # should be steady state and closeby
#'
#'     if (is_steady(q) && is_nearby1(s_current, s_tentative)) {
#'
#'       stb_tentative <- sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = odes)
#'       if (stb_tentative != stb_current) cat("Bifurcation at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'
#'       # Plot continuation
#'       if (identical(log_scale_axis, 'x')) lines(c(p_tentative[p_i]/(1+step),p_tentative[p_i]),c(s_current[s_i],s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'       else lines(c(p_tentative[p_i]-step,p_tentative[p_i]),c(s_current[s_i],s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'
#'       # Update continuation variables
#'       s_previous <- s_current
#'       s_current <- s_tentative
#'       stb_current <- stb_tentative
#'       p_current[p_i] <- p_tentative[p_i]
#'
#'       if (nok > 10 & abs(step) < step_maximum) step <- sign(step)*min(2*abs(step), step_maximum)
#'       nok <- nok + 1
#'
#'       # Logging
#'       iteration <- iteration + 1
#'       add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'     } else {
#'       nok <- 0
#'       if (abs(step) > step_minimum) step <- step / 2
#'       else { # Go back one step, overpredict, call steady, and turn
#'
#'         p_tentative[p_i] <- p_current[p_i]
#'         predState <- s_current + 5*(s_current-s_previous)
#'         q <- rootSolve::steady(y = predState, fun = odes, parms = p_tentative, positive = positive, ...)
#'
#'         s_tentative <- q$y  # should be steady state and not the same
#'         if (is_steady(q) && is_afar1(s_current, s_tentative)) {
#'
#'           cat("Turning point point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'           stb_tentative <- sign_of_dominant_eigenvalue(s = s_tentative, p = p_tentative, m = odes)
#'
#'           middle <- (s_current[s_i] + s_tentative[s_i]) / 2
#'           lines(c(p_tentative[p_i],p_tentative[p_i]),c(s_current[s_i],middle), col=clrs[stb_current+2],lwd=lwds[stb_current+2])
#'           lines(c(p_tentative[p_i],p_tentative[p_i]),c(middle,s_tentative[s_i]), col=clrs[stb_tentative+2],lwd=lwds[stb_tentative+2])
#'           step <- -step
#'           s_previous <- s_current
#'           s_current <- s_tentative
#'           stb_current <- stb_tentative
#'           p_current[p_i] <- p_tentative[p_i]
#'
#'           # Logging
#'           add_row(bif_env, iteration, parameter = p_current[p_i], state = s_current[s_i], stb_current, step, nok)
#'
#'         } else {
#'           cat("Final point at",names(p_tentative[p_i]),"=",p_tentative[p_i],"\n")
#'           cat("If this looks wrong try changing the step size\n")
#'           break
#'         }
#'       }
#'     }
#'   }
#'   # Remove empty rows
#'   return(bif_env$m[rowSums(is.na(bif_env$m)) != ncol(bif_env$m),])
#' }
#'
#' new_bif_matrix <- function(n) {
#'
#'   m <- matrix(data = NA_real_, nrow = n, ncol = 6L)
#'   colnames(m) <- c('iteration', 'parameter', 'state', 'stability', 'step', 'nok')
#'   return(m)
#' }
#'
#' # modifies matrix m in-place by using the environment e
#' add_row <- function(e, iteration, parameter, state, stability, step, nok, growth = 1000L) {
#'
#'   i <- iteration + 1
#'   if(i > nrow(e$m)) {
#'     m2 <- matrix(data = NA_real_, nrow = growth, ncol = 6L)
#'     e$m <- rbind(e$m, m2)
#'   }
#'
#'   e$m[i, ] <- c(iteration, parameter, state, stability, step, nok)
#'
#' }
