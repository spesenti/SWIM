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
#' @param alpha   Numeric vector, the levels of the stressed VaR.
#' @param s          Numeric, vector, the stressed ES at level
#'                   \code{alpha}.\cr
#'                   If \code{q} and \code{s} are vectors, they must have
#'                   the same length.
#' @param s_ratio    Numeric, vector, the ratio of the stressed ES to
#'                   the baseline ES.\cr
#'                   If \code{q} (\code{q_ratio}) and \code{s_ratio} are vectors,
#'                   they must have the same length.
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
#'      \item \code{type = "ES"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, \code{alpha},
#'    \code{q} and \code{s}.
#'     }
#'     See \code{\link{SWIMw}} for details.


stress_RM_w <- function(x, alpha, s_ratio = NULL, s = NULL, k = 1,
                        normalise = FALSE, h = NULL, gamma = NULL){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (!is.null(s) && !is.null(s_ratio)) stop("Only provide s or s_ratio")
  if (is.null(s) && is.null(s_ratio)) stop("no s or s_ratio defined")
  if (!is.null(gamma)){
    if (!all(sapply(gamma, is.function))) stop("gamma must be a function")
  } else{
    warning("No gamma passed. Using expected shortfall.")
    gamma <- function(x){as.numeric((x >= alpha) / (1 - alpha))}
  }

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
  
  
  # Calculate the risk measure
  if(is.null(s)){s <- .rm(FY.inv.fn(u), gamma(u), u) * s_ratio}
  
  .objective_fn <- function(par){
    # Get ell = F_inv + sum(lam*gamma)
    ell.fn <- function(x){FY.inv.fn(x) + par * gamma(x)}
    
    # Get isotonic projection of ell
    GY.inv <- stats::isoreg(u, ell.fn(u))$yf
    rm.stress <- .rm(GY.inv, gamma(u), u)
    
    # Return RM error
    return(sqrt((s - rm.stress)^2))
  }
  
  # Run optimization
  init.lam <- stats::rnorm(1)
  res <- stats::optim(init.lam, .objective_fn, method = "Brent", lower=-100, upper=100)
  lam <- res$par
  
  # Get ell
  ell.fn <- function(x){FY.inv.fn(x) + lam * gamma(x)}
  ell <- ell.fn(u)

  # Get GY_inv, y_grid
  GY.inv <- stats::isoreg(u, ell)$yf
  left <- min(min(x_data[,k]), GY.inv[4])
  right <- max(max(x_data[,k]), GY.inv[length(GY.inv)-3])
  GY.inv.fn <- stats::approxfun(u, GY.inv, yleft=left-1e-5, yright=right+1e-5)
  y.grid <- seq(from=GY.inv[4], to=GY.inv[length(GY.inv)-3], length.out=500)
  

  # Get GY and gY
  GY.fn <- Vectorize(.inverse(GY.inv.fn, lower=0, upper=1))
  
  dG.inv <- (GY.inv[3:length(GY.inv)] - GY.inv[1:(length(GY.inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG.inv.fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG.inv, rule=2)
  gY.fn <- function(x){1/dG.inv.fn(GY.fn(x))}
  
  # Create SWIMw object
  max_length <- max(length(s), length(alpha))
  type <- rep(list("RM"), length.out = max_length)

  # Get weights
  new.weights <- .get_weights(x_data[,k], y.grid, gY.fn, fY.fn, hY)
  names(new.weights) <- paste("stress", 1:max_length)
  
  # achieved RM
  for(j in 1:max_length){
    RM_achieved <- .rm(GY.inv.fn(u), gamma(u), u)
    # message if the achieved RM is different from the specified stress.
    if(s - RM_achieved > 1e-4) {
      message(paste("Stressed RM specified was", round(s, 4),", stressed RM achieved is", round(RM_achieved, 4)))
      s <- RM_achieved
    }
  }
  
  # Get constraints
  s <- rep(s, length.out = max_length)
  alpha <- rep(alpha, length.out = max_length)
  constr_RM <- cbind("k" = rep(k, length.out = max_length), alpha, s)
  
  for(i in 1:max_length){
    temp_list <- list(as.list(constr_RM[i, ]))
    names(temp_list) <- paste("stress", i)
    constr_RM <- c(constr_RM, temp_list)
  }
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=h, "lam"=lam,
                   "new_weights" = new.weights, "str.fY" = gY.fn, "str.FY" = GY.fn,
                   "str.FY.inv" = GY.inv.fn, "type" = type, "specs" = constr_RM)
  
  return(my_list)
}

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
  g.val <- gY_fn(y_grid)
  # g.val[is.na(g.val)] <- 0
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