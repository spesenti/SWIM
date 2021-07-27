#' Stressing Risk Measure, Mean and Standard Deviation
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfils a
#'     constraint on its risk measure, mean, and standard deviation
#'     evaluated at a given level. Scenario weights are
#'     selected by constrained minimisation of the Wasserstein distance to the
#'     baseline model.
#' @param x       A vector, matrix or data frame
#'     containing realisations of random variables. Columns of \code{x}
#'     correspond to random variables; OR\cr
#'     A \code{SWIMw} object, where \code{x} corresponds to the
#'     underlying data of the \code{SWIMw} object.
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.
#' @param alpha   Numeric vector, the levels of the stressed VaR.
#' @param q          Numeric, vector, the stressed ES at level
#'                   \code{alpha}.\cr
#' @param q_ratio    Numeric, vector, the ratio of the stressed ES to
#'                   the baseline ES.\cr
#' @param new_mean    Numeric, the stressed mean. \cr
#' @param new_sd    Numeric, the stressed sd. \cr
#' @param normalise Logical. If true, values of the columns to be stressed are linearly
#'                  normalised to the unit interval.
#' @param h Function that defines the bandwidth used in KDEs. If null,
#' Silverman's rule will be used..
#' @param gamma Function that defines the gamma of the risk measure. If null,
#' the Expected Shortfall (ES) will be used.
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
#'       \item \code{u}, vector containing the gridspace on [0, 1];
#'       \item \code{lam}, vector containing the lambda's of the optimized model;
#'       \item \code{str.fY}, function defining the densities of the stressed component;
#'       \item \code{str.FY}, function defining the distribution of the stressed component;
#'       \item \code{str.FY.inv}, function defining the quantiles of the stressed component;
#'       \item \code{gamma}, function defining the risk measure;
#'       \item \code{new_weights}, a list of functions, that applied to
#'   the \code{k}th column of \code{x}, generates the vectors of scenario
#'   weights. Each component corresponds to a different stress;
#'      \item \code{type = "RM mean sd"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, \code{alpha},
#'    \code{q}, \code{new_mean}, \code{new_sd} and \code{gamma}.
#'     }
#'     See \code{\link{SWIM}} for details.
#'     
#' @author Zhuomin Mao
#'
#' @examples
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress_wass(type = "RM mean sd", x = x,
#'   alpha = 0.9, q_ratio = 1.05, new_mean=1, new_sd=0.9)
#'   summary.SWIM(res1)
#'
#' ## calling stress_RM_w directly
#' ## stressing "gamma"
#' res2 <- stress_RM_mean_std_w(x = x, alpha = 0.9,
#'   q_ratio = 1.05, new_mean=2.2, new_sd=1.5, k = 2)
#' summary.SWIM(res2)
#'
#' @family stress functions
#' @inherit SWIM references
#' @export
#'
stress_RM_mean_std_w <- function(x, alpha, new_mean, new_sd, q_ratio = NULL, q = NULL, k = 1,
                        normalise = FALSE, h = NULL, gamma = NULL){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("no q or q_ratio defined")
  if (!is.null(gamma)){
    if (!is.function(gamma)) stop("gamma must be a function")
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
  
  # Calculate the mean and std
  mean.base <- .integrate(FY.inv.fn(u), u)
  # Calculate the risk measure
  if(is.null(q)){q <- .rm(FY.inv.fn(u), gamma(u), u) * q_ratio}
  
  .objective_fn <- function(par){
    # Get ell = 1/(1 + lambda2) * (F_inv(u) + lambda_1 + lambda_2 * m + sum{lambda_{k+2} + gamma_k(u)})
    ell.fn <- function(x){(FY.inv.fn(x) + par[1] + par[2]*new_mean + par[3]*gamma(x))/(1 + par[2])}
    
    # Get isotonic projection of ell
    GY.inv <- stats::isoreg(u, ell.fn(u))$yf
    mean.stress <- .integrate(GY.inv, u)
    std.stress <- sqrt(.integrate((GY.inv - mean.stress)^2, u))
    rm.stress <- .rm(GY.inv, gamma(u), u)
    
    # Return RM error
    error <- sqrt(2*(mean.stress - new_mean)^2 + 
                  2*(std.stress - new_sd)^2 +
                  (q - rm.stress)^2)
    # sqrt(2) normalization constant
    return(error)
  }
  
  # Run optimization
  init.lam <- stats::rnorm(3)
  res <- stats::optim(init.lam, .objective_fn, method = "Nelder-Mead")
  lam <- res$par
  
  # Get ell
  ell.fn <- function(x){(FY.inv.fn(x) + lam[1] + lam[2]*new_mean + lam[3]*gamma(x))/(1 + lam[2])}
  ell <- ell.fn(u)

  # Get GY_inv, y_grid
  GY.inv <- stats::isoreg(u, ell)$yf
  left <- min(min(x_data[,k]), GY.inv[4])
  right <- max(max(x_data[,k]), GY.inv[length(GY.inv)-3])
  GY.inv.fn <- stats::approxfun(u, GY.inv, yleft=left-1e-5, yright=right+1e-5)
  y.grid <- seq(from=GY.inv[4], to=GY.inv[length(GY.inv)-3], length.out=500)
  
  # Get GY and gY
  GY.fn <- Vectorize(.inverse(GY.inv.fn, lower=min(u), upper=max(u)))
  
  dG.inv <- (GY.inv[3:length(GY.inv)] - GY.inv[1:(length(GY.inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG.inv.fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG.inv)
  gY.fn <- function(x){1/dG.inv.fn(GY.fn(x))}
  
  # Create SWIMw object
  max_length <- max(length(new_mean), length(new_sd), length(q), length(alpha))
  type <- rep(list("mean-std"), length.out = max_length)

  # Get weights
  new.weights <- .get_weights(x_data[,k], y.grid, gY.fn, fY.fn, hY)
  names(new.weights) <- paste("stress", 1:max_length)
  
  # achieved mean and std
  for(j in 1:max_length){
    mean.achieved <- .integrate(GY.inv, u)
    sd.achieved <- sqrt(.integrate((GY.inv - mean.achieved)^2, u))
    RM_achieved <- .rm(GY.inv.fn(u), gamma(u), u)
    
    # message if the achieved RM is different from the specified stress.
    if(q - RM_achieved > 1e-4) {
      message(paste("Stressed RM specified was", round(q, 4),", stressed RM achieved is", round(RM_achieved, 4)))
      q <- RM_achieved
    }
    
    # message if the achieved mean or std is different from the specified stress.
    if(mean.achieved - new_mean > 1e-4) {
      message(paste("Stressed mean specified was", round(new_mean, 4),", stressed mean achieved is", round(mean.achieved, 4)))
      new_mean <- mean.achieved
    }
    if(sd.achieved - new_sd > 1e-4) {
      message(paste("Stressed std specified was", round(new_sd, 4),", stressed std achieved is", round(sd.achieved, 4)))
      new_sd <- sd.achieved
    }
  }
  
  # Get constraints
  new_mean <- rep(new_mean, length.out = max_length)
  new_sd <- rep(new_sd, length.out = max_length)
  q <- rep(q, length.out = max_length)
  alpha <- rep(alpha, length.out = max_length)
  constr_RM_mean_std <- cbind("k" = rep(k, length.out = max_length), alpha, q, new_mean, new_sd)
  
  constr <- list()
  for(i in 1:max_length){
    temp_list <- list(as.list(constr_RM_mean_std[i, ]))
    names(temp_list) <- paste("stress", i)
    constr <- c(constr, temp_list)
  }
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=h, "lam"=lam, "gamma" = gamma,
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
  return(function(y){stats::uniroot((function(x){f(x) - y}), lower = lower, upper = upper, extendInt = 'yes')$root})
}

.integrate <- function(f, x){
  return(sum(0.5*(f[1:length(f) - 1] + f[2:length(f)])*diff(x)))
}

.rm <- function(F_inv, gamma, u){
  return(.integrate(F_inv*gamma, u))
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