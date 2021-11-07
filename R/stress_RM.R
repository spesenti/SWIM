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
#'     underlying data of the \code{SWIMw} object. The stressed random component is assumed continuously distributed.
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.\cr
#' @param alpha   Numeric, vector, the levels of the stressed risk measure (RM).\cr
#' @param q          Numeric, vector, the stressed RM at level
#'                   \code{alpha} (must be same length as \code{alpha}).\cr
#' @param q_ratio    Numeric, vector, the ratio of the stressed RM to
#'                   the baseline RM (must be same length as \code{alpha}).\cr
#' @param normalise Logical. If true, values of the columns to be stressed are linearly
#'                  normalised to the unit interval (\code{default = FALSE}).\cr
#' @param h Function that defines the bandwidth used in KDE (\code{default = }
#' Silverman's rule).\cr
#' @param gamma Function that defines the gamma of the risk measure which takes in 
#' \code{alpha} as an argument (\code{default Expected Shortfall}).
#' @param names   Character vector, the names of stressed models.
#' @param log     Boolean, the option to print weights' statistics.
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
#' \dontrun{
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
#' }
#'
#' @family stress functions
#' @inherit SWIM references
#' @export
#'
stress_RM_w <- function(x, alpha, q_ratio = NULL, q = NULL, k = 1,
                        normalise = FALSE, h = NULL, gamma = NULL, names = NULL, log = FALSE){
  t0 <- Sys.time()
  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("no q or q_ratio defined")
  if (!is.null(q) && (length(q) != length(alpha))) stop("q and alpha must have the same length")
  if (!is.null(q_ratio) && (length(q_ratio) != length(alpha))) stop("q_ratio and alpha must have the same length")
  if (!is.null(gamma)){
    if (!is.function(gamma)) stop("gamma must be a function")
  } else{
    warning("No gamma passed. Using expected shortfall.")
    gamma <- function(x, alpha){as.numeric((x >= alpha) / (1 - alpha))}
  }
  
  n <- length(x_data[, k])
  
  # Create grid u in [0,1]
  u <- c(.ab_grid(1e-4, 0.05, 100), .ab_grid(0.05, 0.99, 500), .ab_grid(0.99, 1-1e-4, 100))
  
  # Get the KDE estimates for fY, FY
  print("Get the KDE estimates for fY, FY")
  if (is.null(h)){
    # Use Silverman's Rule
    h <- function(y){1.06 * stats::sd(y) * length(y)^(-1/5)}
  } else if (!is.function(h)) {
    stop("Please pass a function calculating the bandwidth, or use the default bandwidth (pass NULL to h)")
  }
  hY <- h(x_data[,k])
  
  # kde_f <- function(y) {
  #   fY_fn_wrapper <- function(y) snpar::kde(x_data[,k], hY, y)$fhat
  #   fY_fn_wrapper <- Vectorize(fY_fn_wrapper)
  #   FY_fn_wrapper <- function(y) snpar::kde(x_data[,k], hY, y)$Fhat
  #   FY_fn_wrapper <- Vectorize(FY_fn_wrapper)
  #   return (list(fY_fn_wrapper, FY_fn_wrapper))
  # }
  
  fY_fn <- function(y){
    return(sum(stats::dnorm((y - x_data[,k])/hY)/hY/length(x_data[,k])))
  }
  fY_fn <- Vectorize(fY_fn)

  FY_fn <- function(y){
    return(sum(stats::pnorm((y - x_data[,k])/hY)/length(x_data[,k])))
  }
  FY_fn <- Vectorize(FY_fn)
  
  # fY_fn <- kde_f(y)[[1]]
  # FY_fn <- kde_f(y)[[2]]
  
  lower_bracket = min(x_data[,k])-(max(x_data[,k])-min(x_data[,k]))*0.1
  upper_bracket = max(x_data[,k])+(max(x_data[,k])-min(x_data[,k]))*0.1
  FY_inv_fn <- Vectorize(.inverse(FY_fn, lower_bracket, upper_bracket))
  print(Sys.time() - t0)
  print("Done calculating KDE")
  
  # Calculate the risk measure
  print("Calculate the risk measure")
  if(is.null(q)){
    q = c()
    for (i in 1:length(q_ratio)){
      q <- append(q, .rm(FY_inv_fn(u), gamma(u, alpha[i]), u) * q_ratio[i])
    }
  }
  
  .objective_fn <- function(par){
    # Get ell = F_inv + sum(lam*gamma)
    ell_fn <- function(x){
      ell <- FY_inv_fn(x)
      for (i in 1:length(par)){
        ell <- ell + par[i]*gamma(x, alpha[i])
      }
      return(ell)
    }
    
    # Get isotonic projection of ell
    GY_inv <- stats::isoreg(u, ell_fn(u))$yf
    
    # Return RM error
    rm_stress <- c()
    for (i in 1:length(alpha)){
      rm_stress <- append(rm_stress, .rm(GY_inv, gamma(u, alpha[i]), u))
    }
    return(sqrt(sum(q - rm_stress)^2))
  }
  print(Sys.time() - t0)

  # Run optimization
  print("Run optimization")
  max_length <- length(q)
  init_lam <- stats::rnorm(max_length)
  res <- stats::optim(init_lam, .objective_fn, method = "Nelder-Mead")
  lam <- res$par
  print(Sys.time() - t0)
  print("Optimization converged")
  
  # Get ell
  ell_fn <- function(x){
    ell <- FY_inv_fn(x)
    for (i in 1:length(lam)){
      ell <- ell + lam[i]*gamma(x, alpha[i])
    }
    return(ell)
  }
  ell <- ell_fn(u)
  
  # Get GY_inv, y_grid
  print("Calculate optimal quantile function")
  GY_inv <- stats::isoreg(u, ell)$yf
  left <- min(min(x_data[,k]), GY_inv[4])
  right <- max(max(x_data[,k]), GY_inv[length(GY_inv)-3])
  GY_inv_fn <- stats::approxfun(u, GY_inv, yleft=left-1e-5, yright=right+1e-5)
  y_grid <- seq(from=GY_inv[4], to=GY_inv[length(GY_inv)-3], length.out=500)
  print(Sys.time() - t0)
  
  # Get GY and gY
  print("Calculate optimal cdf and density")
  GY_fn <- Vectorize(.inverse(GY_inv_fn, lower=0, upper=1))
  
  dG_inv <- (GY_inv[3:length(GY_inv)] - GY_inv[1:(length(GY_inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG_inv_fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG_inv, rule=2)
  gY_fn <- function(x){1/dG_inv_fn(GY_fn(x))}
  print(Sys.time() - t0)
  
  # Create SWIMw object
  type <- list("RM")
  
  # Get weights
  new_weights <- .get_weights(x_data[,k], y_grid, gY_fn, fY_fn, hY)

  # Name stresses
  if (is.null(names)) {
    temp <- paste("stress", 1)
  } else {
    temp <- names
  }
  if (length(temp) != 1) stop("length of names are not the same as the number of models")
  names(new_weights) <- temp
  
  # achieved RM
  for(j in 1:max_length){
    RM_achieved <-c()
    for (i in 1:length(alpha)){
      RM_achieved <- append(RM_achieved, .rm(GY_inv_fn(u), gamma(u, alpha[i]), u))
    }
    # message if the achieved RM is different from the specified stress.
    if(any(q - RM_achieved > 1e-4)) {
      message(paste("Stressed RM specified was ", round(q, 4),", stressed RM achieved is ", round(RM_achieved, 4)))
      q <- RM_achieved
    }
  }
  
  # Get constraints
  constr <- list(list("k"=k, "q"=q, "alpha"=alpha))
  names(constr) <- temp
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=list(h), "lam"=list(lam), "gamma" = list(gamma),
                   "new_weights" = new_weights, "str_fY" = list(gY_fn), "str_FY" = list(GY_fn),
                   "str_FY_inv" = list(GY_inv_fn), "type" = type, "specs" = constr)
  
  ## use for plotting the different kdes
  # plot(GY_inv_fn, lim = c(-3,3))
  # axis(side=2, at=seq(-3, 3, by=1))
  # return (list(GY_inv_fn))
  
  if (is.SWIMw(x)) my_list <- merge(x, my_list)
  
  if (log) {
    summary_weights(my_list)
  }
  
  return(my_list)
}

