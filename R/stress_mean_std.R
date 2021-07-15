#' Stressing Value-at-Risk and Expected Shortfall
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfils a
#'     constraint on its Expected Shortfall (ES) risk
#'     measure, evaluated at a given level. Scenario weights are
#'     selected by constrained minimisation of the relative entropy to the
#'     baseline model.
#' @param x       A vector, matrix or data frame
#'     containing realisations of random variables. Columns of \code{x}
#'     correspond to random variables; OR\cr
#'     A \code{SWIM} object, where \code{x} corresponds to the
#'     underlying data of the \code{SWIM} object.
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.
#' @param mean_ratio    Numeric, the ratio of the stressed mean to
#'                   the baseline mean\cr
#' @param std_ratio    Numeric, the ratio of the stressed std to
#'                   the baseline std\cr
#' @param normalise Logical. If true, values of the columns to be stressed are linearly
#'                  normalised to the unit interval.
#'
#' @details The ES at level \code{alpha} of a random variable with distribution
#'     function F is defined by:
#'     \deqn{ES_{alpha} = 1 / (1 - alpha) * \int_{alpha}^1 VaR_u d u.}
#'
#'
#' @return A \code{SWIMw} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{h}, bandwidths;
#'       \item \code{new_weights}, a list of functions, that applied to
#'   the \code{k}th column of \code{x}, generates the vectors of scenario
#'   weights. Each component corresponds to a different stress;
#'      \item \code{type = "mean-std"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, and \code{s}.
#'     }
#'     See \code{\link{SWIMw}} for details.


stress_mean_std_w <- function(x, mean_ratio, std_ratio, k = 1,
                        normalise = FALSE, h = NULL){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")

  n <- length(x_data[, k])
  
  # Create grid u in [0,1]
  u <- c(.ab_grid(1e-4, 0.05, 100), .ab_grid(0.05, 0.99, 500), .ab_grid(0.99, 1-1e-4, 100))
  
  # Get the KDE estimates for fY, FY
  if (is.null(h)){
    # Use Silverman's Rule
    h <- function(y){1.06 * sd(y) * length(y)^(-1/5)}
  } else if (!is.function(h)) {
    stop("Please pass a function calculating the bandwidth, or use the default bandwidth (pass NULL to h)")
  }
  hY <- h(x_data[,k])
  
  fY.fn <- function(y){
    return(sum(dnorm((y - x_data[,k])/hY)/hY/length(x_data[,k])))
  }
  fY.fn <- Vectorize(fY.fn)
  FY.fn <- function(y){
    return(sum(pnorm((y - x_data[,k])/hY)/length(x_data[,k])))
  }
  FY.fn <- Vectorize(FY.fn)
  lower.bracket = min(x_data[,k])-(max(x_data[,k])-min(x_data[,k]))*0.1
  upper.bracket = max(x_data[,k])+(max(x_data[,k])-min(x_data[,k]))*0.1
  FY.inv.fn <- Vectorize(.inverse(FY.fn, lower.bracket, upper.bracket))
  
  # Calculate the mean and std
  mean.base <- .integrate(FY.inv.fn(u), u)
  mean.target <- mean.base * mean_ratio
  std.target <- sqrt(.integrate((FY.inv.fn(u) - mean.base)^2, u)) * std_ratio
  
  .objective_fn <- function(par){
    # Get ell = F_inv + sum(lam[1] + lam[2]*mean)
    ell.fn <- function(x){(FY.inv.fn(x) + par[1] + par[2]*mean.target)/(1 + par[2])}
    
    # Get isotonic projection of ell
    GY.inv <- stats::isoreg(u, ell.fn(u))$yf
    mean.stress <- .integrate(GY.inv, u)
    std.stress <- sqrt(.integrate((GY.inv - mean.stress)^2, u))
    
    # Return RM error
    error <- sqrt(2) * sqrt((mean.stress - mean.target)^2 + 
                              (std.stress - std.target)^2)  # sqrt(2) normalization constant
    return(error)
  }
  
  # Run optimization
  init.lam <- stats::rnorm(2)
  res <- stats::optim(init.lam, .objective_fn, method = "Nelder-Mead")
  lam <- res$par
  
  # Get ell
  ell.fn <- function(x){(FY.inv.fn(x) + lam[1] + lam[2]*mean.target)/(1 + lam[2])}
  ell <- ell.fn(u)

  # Get GY_inv, y_grid
  GY.inv <- stats::isoreg(u, ell)$yf
  GY.inv.fn <- stats::approxfun(u, GY.inv)
  y.grid <- seq(from=GY.inv[4], to=GY.inv[length(GY.inv)-3], length.out=500)
  
  # Get GY and gY
  GY.fn <- Vectorize(.inverse(GY.inv.fn, lower=min(u), upper=max(u)))
  
  dG.inv <- (GY.inv[3:length(GY.inv)] - GY.inv[1:(length(GY.inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG.inv.fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG.inv)
  gY.fn <- function(x){1/dG.inv.fn(GY.fn(x))}
  
  # Create SWIMw object
  max_length <- max(length(mean_ratio), length(std_ratio))
  type <- rep(list("mean-std"), length.out = max_length)

  # Get weights
  new.weights <- .get_weights(x_data[,k], y.grid, gY.fn, fY.fn, hY)
  names(new.weights) <- paste("stress", 1:max_length)
  
  # achieved mean and std
  for(j in 1:max_length){
    mean.achieved <- .integrate(GY.inv, u)
    std.achieved <- sqrt(.integrate((GY.inv - mean.achieved)^2, u))
    
    # message if the achieved mean or std is different from the specified stress.
    if(mean.achieved - mean.target > 1e-4) {
      message(paste("Stressed mean specified was", round(mean.target, 4),", stressed mean achieved is", round(mean.achieved, 4)))
      mean.target <- mean.achieved
    }
    if(std.achieved - std.target > 1e-4) {
      message(paste("Stressed std specified was", round(std.target, 4),", stressed std achieved is", round(std.achieved, 4)))
      std.target <- std.achieved
    }
  }
  
  # Get constraints
  mean.target <- rep(mean.target, length.out = max_length)
  std.target <- rep(std.target, length.out = max_length)
  constr <- cbind("k" = rep(k, length.out = max_length), mean_ratio, std_ratio)
  
  for(i in 1:max_length){
    temp_list <- list(as.list(constr[i, ]))
    names(temp_list) <- paste("stress", i)
    constr <- c(constr, temp_list)
  }
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=h, "lam"=lam,
                   "new_weights" = new.weights, "str.fY" = gY.fn, "str.FY" = GY.fn,
                   "str.FY.inv" = GY.inv.fn, "type" = type, "specs" = constr)
  
  return(my_list)
}

# helper functions
.ab_grid <- function(a, b, N){
  eps <- 0.002
  u_eps <- 10^(seq(from=-10, to=log10(eps), length.out=10)) - 1e-11
  return(c(a + u_eps, seq(from=a + eps, to=b - eps, length.out=N), b - rev(u_eps)))
}

.inverse <- function(f, lower = -100, upper = 100){
  return(function(y){stats::uniroot((function(x){f(x) - y}), lower = lower, upper = upper)$root})
}

.integrate <- function(f, x){
  return(sum(0.5*(f[1:length(f) - 1] + f[2:length(f)])*diff(x)))
}

.get_weights <- function(y_data, y_grid, gY_fn, fY_fn, hY){
  # Get dQ/dP
  g.val <- gY_fn(y_grid)
  g.val[is.na(g.val)] <- 0
  g.val <- g.val/.integrate(g.val, y_grid)
  f.val <- fY_fn(y_grid)/.integrate(fY_fn(y_grid), y_grid)
  dQ.dP <- g.val / f.val
  
  # Get weights
  w <- vector()
  for(i in 1:length(y_data)){
    w <- c(w, .integrate(dQ.dP*stats::dnorm((y_grid - y_data[i])/hY)/hY, y_grid))
  }
  # Normalize weights
  w <- w / sum(w) * length(y_data)
  
  return(list(w))
}