# helper functions
.ab_grid <- function(a, b, N){
  eps <- 0.002
  u_eps <- 10^(seq(from=-10, to=log10(eps), length.out=10)) - 1e-11
  return(c(a + u_eps, seq(from=a + eps, to=b - eps, length.out=N), b - rev(u_eps)))
}

.inverse <- function(f, lower = -100, upper = 100){
  return(function(y){stats::uniroot((function(x){f(x) - y}), lower = lower, upper = upper, extendInt = 'yes')$root})
}

.rm <- function(F_inv, gamma, u){
  return(.integrate(F_inv*gamma, u))
}

.integrate <- function(f, x){
  return(sum(0.5*(f[1:length(f) - 1] + f[2:length(f)])*diff(x)))
}

.get_weights <- function(y_data, y_grid, gY_fn, fY_fn, hY){
  # Get dQ/dP
  g_val <- gY_fn(y_grid)
  # g.val[is.na(g.val)] <- 0
  g_val <- g_val/.integrate(g_val, y_grid)
  f_val <- fY_fn(y_grid)/.integrate(fY_fn(y_grid), y_grid)
  dQ_dP <- g_val / f_val
  
  # Get weights
  w <- vector()
  for(i in 1:length(y_data)){
    w <- c(w, .integrate(dQ_dP*stats::dnorm((y_grid - y_data[i])/hY)/hY, y_grid))
  }
  # Normalize weights
  w <- w / sum(w) * length(y_data)
  
  return(list(w))
}

.hara_utility<- function(a, b, eta, u, F_inv){
  # f = (1 - eta) / eta * (a * F_inv / (1 - eta) + b) ^ eta
  dummy = a * F_inv / (1 - eta) + b
  f = (1 - eta) / eta * sign(dummy) * abs(dummy) ^ eta
  return(.integrate(f, u))
}

.utransform <- function(a, b, eta, u, G_inv, lam, upper){
  g <- c()
  nu <- function(x) x - lam * a * (a / (1 - eta) * x + b) ** (eta - 1)
  
  for (i in 1:length(u)){
    val <- stats::uniroot((function(x){nu(x) - G_inv[i]}), 
                          lower = -b*(1-eta)/a + 1e-10, upper = upper)$root
    g <- append(g, val)
  }
  return(g)
}