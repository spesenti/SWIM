#' Stressing Risk Measure
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfills a
#'     constraint on its risk measure defined by a \code{gamma} function and 
#'     evaluated at a given level \code{alpha}. Scenario weights are
#'     selected by constrained minimisation of the Wasserstein distance to the
#'     baseline model.
#' @param x       A vector, matrix or data frame
#'     containing realisations of random variables. Columns of \code{x}
#'     correspond to random variables; OR\cr
#'     A \code{SWIMw} object, where \code{x} corresponds to the
#'     underlying data of the \code{SWIMw} object.
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.\cr
#' @param alpha   Numeric vector, the levels of the stressed risk measure (RM).\cr
#' @param q          Numeric, vector, the stressed RM at level
#'                   \code{alpha}.\cr
#' @param q_ratio    Numeric, vector, the ratio of the stressed RM to
#'                   the baseline RM.\cr
#' @param normalise Logical. If true, values of the columns to be stressed are linearly
#'                  normalised to the unit interval (\code{default = FALSE}).\cr
#' @param h Function that defines the bandwidth used in KDE (\code{default = }
#' Silverman's rule).\cr
#' @param gamma Function that defines the gamma of the risk measure 
#' (\code{default =} Expected Shortfall).
#'
#' @details This function implements stresses on distortion risk measures.
#'     Distortion risk measures are defined by a square-integrable function
#'     \code{gamma} where
#'     \deqn{\int_0^1 gamma(u) du = 1.}
#'     
#'     The distortion risk measure for some \code{gamma} and distribution 
#'     \code{G} is calculated as:
#'     \deqn{\rho_{gamma}(G) = \int_0^1 \breve(G)(u) gamma(u) du.}
#' 
#'     Expected Shortfall (ES) is an example of a distortion risk measure.
#'     The ES at level \code{alpha} of a random variable with distribution
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
#'       \item \code{str_fY}, function defining the densities of the stressed component;
#'       \item \code{str_FY}, function defining the distribution of the stressed component;
#'       \item \code{str_FY_inv}, function defining the quantiles of the stressed component;
#'       \item \code{gamma}, function defining the risk measure;
#'       \item \code{new_weights}, a list of functions, that applied to
#'   the \code{k}th column of \code{x}, generates the vectors of scenario
#'   weights. Each component corresponds to a different stress;
#'      \item \code{type = "RM"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, \code{alpha}, and
#'    \code{q}.
#'     }
#'     See \code{\link{SWIM}} for details.
#' @author Zhuomin Mao
#'
#' @examples
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress_wass(type = "RM", x = x,
#'   alpha = 0.9, q_ratio = 1.05)
#'   summary(res1)
#'
#' ## calling stress_RM_w directly
#' ## stressing "gamma"
#' res2 <- stress_RM_w(x = x, alpha = 0.9,
#'   q_ratio = 1.05, k = 2)
#' summary(res2)
#'
#' @family stress functions
#' @inherit SWIM references
#' @export
#'
stress_RM_w <- function(x, alpha, q_ratio = NULL, q = NULL, k = 1,
                        normalise = FALSE, h = NULL, gamma = NULL){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
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
  
  fY_fn <- function(y){
    return(sum(dnorm((y - x_data[,k])/hY)/hY/length(x_data[,k])))
  }
  fY_fn <- Vectorize(fY_fn)
  FY_fn <- function(y){
    return(sum(pnorm((y - x_data[,k])/hY)/length(x_data[,k])))
  }
  FY_fn <- Vectorize(FY_fn)
  lower_bracket = min(x_data[,k])-(max(x_data[,k])-min(x_data[,k]))*0.1
  upper_bracket = max(x_data[,k])+(max(x_data[,k])-min(x_data[,k]))*0.1
  FY_inv_fn <- Vectorize(.inverse(FY_fn, lower_bracket, upper_bracket))
  
  
  # Calculate the risk measure
  if(is.null(q)){q <- .rm(FY_inv_fn(u), gamma(u), u) * q_ratio}
  
  .objective_fn <- function(par){
    # Get ell = F_inv + sum(lam*gamma)
    ell_fn <- function(x){FY_inv_fn(x) + par * gamma(x)}
    
    # Get isotonic projection of ell
    GY_inv <- stats::isoreg(u, ell_fn(u))$yf
    rm_stress <- .rm(GY_inv, gamma(u), u)
    
    # Return RM error
    return(sqrt((q - rm_stress)^2))
  }
  
  # Run optimization
  init_lam <- stats::rnorm(1)
  res <- stats::optim(init_lam, .objective_fn, method = "Nelder-Mead")
  lam <- res$par
  
  # Get ell
  ell_fn <- function(x){FY_inv_fn(x) + lam * gamma(x)}
  ell <- ell_fn(u)

  # Get GY_inv, y_grid
  GY_inv <- stats::isoreg(u, ell)$yf
  left <- min(min(x_data[,k]), GY_inv[4])
  right <- max(max(x_data[,k]), GY_inv[length(GY_inv)-3])
  GY_inv_fn <- stats::approxfun(u, GY_inv, yleft=left-1e-5, yright=right+1e-5)
  y_grid <- seq(from=GY_inv[4], to=GY_inv[length(GY_inv)-3], length.out=500)
  

  # Get GY and gY
  GY_fn <- Vectorize(.inverse(GY_inv_fn, lower=0, upper=1))
  
  dG_inv <- (GY_inv[3:length(GY_inv)] - GY_inv[1:(length(GY_inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG_inv_fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG_inv, rule=2)
  gY_fn <- function(x){1/dG_inv_fn(GY_fn(x))}
  
  # Create SWIMw object
  max_length <- max(length(q), length(alpha))
  type <- rep(list("RM"), length.out = max_length)

  # Get weights
  new_weights <- .get_weights(x_data[,k], y_grid, gY_fn, fY_fn, hY)
  names(new_weights) <- paste("stress", 1:max_length)
  
  # achieved RM
  for(j in 1:max_length){
    RM_achieved <- .rm(GY_inv_fn(u), gamma(u), u)
    # message if the achieved RM is different from the specified stress.
    if(q - RM_achieved > 1e-4) {
      message(paste("Stressed RM specified was", round(q, 4),", stressed RM achieved is", round(RM_achieved, 4)))
      q <- RM_achieved
    }
  }
  
  # Get constraints
  q <- rep(q, length.out = max_length)
  alpha <- rep(alpha, length.out = max_length)
  constr_RM <- cbind("k" = rep(k, length.out = max_length), alpha, q)

  constr <- list()
  for(i in 1:max_length){
    temp_list <- list(as.list(constr_RM[i, ]))
    names(temp_list) <- paste("stress", i)
    constr <- c(constr, temp_list)
  }
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=h, "lam"=lam, "gamma" = gamma, 
                   "new_weights" = new_weights, "str_fY" = gY_fn, "str_FY" = GY_fn,
                   "str_FY_inv" = GY_inv_fn, "type" = type, "specs" = constr)
  
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
