library(fluents)
library(tidyverse)


m <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    dR <- r * R * (1 - R / K) - a * R * N
    dN <- c * a * R * N - delta * N

    return(list(c(dR, dN)))
  })
}

p <- c(r = 1, K = 1, a = 1, c = 1, delta = 0.5) # p is a named vector of parameters
s <- c(R = 0.51, N = 0.01)                # s is the state
s <- c(R = 0.5, N = 0)                # s is the state
t <- seq(0, 100, by= 1)

sol1 <- solve(s = s, p = p, m = m, t = t)

ggplot(data = sol1, mapping = aes(x = t)) +
  geom_line(aes(y = R), col = "red") +
  geom_line(aes(y = N), col = "blue")



foo <- (find_steady_state(s0 = s, p = p, m = m))
ss <- steady(y = s0, fun = m, parms = p)

m <- function(t, state, parms) {
  with(as.list(c(state,parms)), {

    dR <- r*t*(1 - t/K) - a*t*N
    dN <- c*a*t*N - delta*N

    return(list(c(dR, dN)))
  })
}

p <- c(r=1,K=1,a=1,c=1,delta=0.5) # p is a named vector of parameters
s <- c(t=1,N=0.01)                # s is the state
t <- seq(0, 100, by= 1)

sol1 <- solve(s = s, p = p, m = m, t = t)
