#' Stressing Risk Measure, Mean and Standard Deviation
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfils a
#'     constraint on its mean, standard deviation, and risk measure 
#'     defined by a \code{gamma} function and 
#'     evaluated at a given level \code{alpha}. Scenario weights are
#'     selected by constrained minimisation of the Wasserstein distance to the
#'     baseline model.
#' @inheritParams    stress_RM_w
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.
#' @param q          Numeric, vector, the stressed RM at level
#'                   \code{alpha} (must be same length as \code{alpha}).\cr
#' @param q_ratio    Numeric, vector, the ratio of the stressed RM to
#'                   the baseline RM (must be same length as \code{alpha}).\cr
#' @param new_means    Numeric, the stressed mean. \cr
#' @param new_sd    Numeric, the stressed standard deviation. \cr
#' @param names   Character vector, the names of stressed models.
#' @param log     Boolean, the option to print weights' statistics.
#' @param ...       Additional arguments to be passed to
#'                  \code{\link[nleqslv]{nleqslv}}.
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
#'       \item \code{h}, h is a multiple of the Silverman’s rule;
#'       \item \code{u}, vector containing the gridspace on [0, 1];
#'       \item \code{lam}, vector containing the lambda's of the optimized model;
#'       \item \code{str_fY}, function defining the densities of the stressed component;
#'       \item \code{str_FY}, function defining the distribution of the stressed component;
#'       \item \code{str_FY_inv}, function defining the quantiles of the stressed component;
#'       \item \code{gamma}, function defining the risk measure;
#'       \item \code{new_weights}, a list of functions, that applied to
#'   the \code{k}th column of \code{x}, generates the vectors of scenario
#'   weights. Each component corresponds to a different stress;
#'      \item \code{type = "RM mean sd"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, \code{alpha},
#'    \code{q}, \code{new_means}, and \code{new_sd}.
#'     }
#'     See \code{\link{SWIM}} for details.
#'     
#' @author Zhuomin Mao
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress_wass(type = "RM mean sd", x = x,
#'   alpha = 0.9, q_ratio = 1.05, new_means=1, new_sd=0.9)
#'   summary(res1)
#'
#' ## calling stress_RM_w directly
#' ## stressing "gamma"
#' res2 <- stress_RM_mean_sd_w(x = x, alpha = 0.9,
#'   q_ratio = 1.05, new_means=2.2, new_sd=1.5, k = 2)
#' summary(res2)
#' }
#'
#' @family stress functions
#' @references \insertRef{Pesenti2019reverse}{SWIM}\cr
#'
#'     \insertRef{Pesenti2020SSRN}{SWIM}\cr
#'
#'     \insertRef{Pesenti2021SSRN}{SWIM}
#' @export
#'
stress_RM_mean_sd_w <- function(x, alpha = 0.8, new_means, new_sd, q_ratio = NULL, q = NULL, k = 1,
                                h = 1, gamma = NULL, names = NULL, log = FALSE, method = "Nelder-Mead", ...){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("no q or q_ratio defined")

  if (!is.null(gamma)){
    if (!is.function(gamma)) stop("gamma must be a function")
    else {
      # if (!is.null(alpha)) stop("Both gamma and alpha are provided")
      .gamma <- function(x, alpha = alpha){gamma(x)}
    }
  } else{
    warning("No gamma passed. Using expected shortfall.")
    if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
    if (!is.null(q) && (length(q) != length(alpha))) stop("q and alpha must have the same length")
    if (!is.null(q_ratio) && (length(q_ratio) != length(alpha))) stop("q_ratio and alpha must have the same length")
    .gamma <- function(x, alpha = alpha){as.numeric((x >= alpha) / (1 - alpha))}
  }
  
  n <- length(x_data[, k])
  
  # Create grid u in [0,1]
  u <- c(.ab_grid(1e-4, 0.05, 100), .ab_grid(0.05, 0.99, 500), .ab_grid(0.99, 1-1e-4, 100))
  
  # Get the KDE estimates for fY, FY
  print("Get the KDE estimates")
  if (is.numeric(h)){
    # Use Silverman's Rule
    .h <- function(y){
      h * 1.06 * stats::sd(y) * length(y)^(-1/5)
    }
  } else {
    stop("h must be numeric")
  }
  hY <- .h(x_data[,k])
  
  fY_fn <- function(y){
    return(sum(stats::dnorm((y - x_data[,k])/hY)/hY/length(x_data[,k])))
  }
  fY_fn <- Vectorize(fY_fn)
  FY_fn <- function(y){
    return(sum(stats::pnorm((y - x_data[,k])/hY)/length(x_data[,k])))
  }
  FY_fn <- Vectorize(FY_fn)
  lower_bracket = min(x_data[,k])-(max(x_data[,k])-min(x_data[,k]))*0.1
  upper_bracket = max(x_data[,k])+(max(x_data[,k])-min(x_data[,k]))*0.1
  FY_inv_fn <- Vectorize(.inverse(FY_fn, lower_bracket, upper_bracket))

  # Calculate the mean and sd
  mean.base <- .integrate(FY_inv_fn(u), u)

  # Calculate the risk measure
  if(is.null(q)){
    q = c()
    for (i in 1:length(q_ratio)){
      q <- append(q, .rm(FY_inv_fn(u), .gamma(u, alpha[i]), u) * q_ratio[i])
    }
  }
  
  .objective_fn <- function(par){
    # Get ell = 1/(1 + lambda2) * (F_inv(u) + lambda_1 + lambda_2 * m + sum{lambda_{k+2} + gamma_k(u)})
    ell_fn <- function(x){
      ell <- FY_inv_fn(x) + par[1] + par[2]*new_means
      for (i in 1:(length(par)-2)){
        ell <- ell + par[i+2]*.gamma(x, alpha[i])
      }
      ell <- ell / (1 + par[2])
      return(ell)
    }
    
    # Get isotonic projection of ell
    GY_inv <- stats::isoreg(u, ell_fn(u))$yf
    mean_stress <- .integrate(GY_inv, u)
    sd_stress <- sqrt(.integrate((GY_inv - mean_stress)^2, u))
    rm_stress <-c()
    for (i in 1:length(alpha)){
      rm_stress <- append(rm_stress, .rm(GY_inv, .gamma(u, alpha[i]), u))
    }
    
    # Return error
    error <- sqrt(2*(mean_stress - new_means)^2 + 
                  2*(sd_stress - new_sd)^2 +
                  sum((q - rm_stress)^2))
    # sqrt(2) normalization constant
    return(error)
  }
  
  # Run optimization
  print("Run optimization")
  max_length <- length(q)
  init_lam <- stats::rnorm(2+max_length)
  res <- stats::optim(init_lam, .objective_fn, method = method)
  lam <- res$par
  print("Optimization converged")
  
  # Get ell
  ell_fn <- function(x){
    ell <- FY_inv_fn(x) + lam[1] + lam[2]*new_means
    for (i in 1:(length(lam)-2)){
      ell <- ell + lam[i+2]*.gamma(x, alpha[i])
    }
    ell <- ell / (1 + lam[2])
    return(ell)
  }
  ell <- ell_fn(u)

  print("Calculate optimal scenario weights")
  # Get GY_inv, y_grid
  GY_inv <- stats::isoreg(u, ell)$yf
  left <- min(min(x_data[,k]), GY_inv[4])
  right <- max(max(x_data[,k]), GY_inv[length(GY_inv)-3])
  GY_inv_fn <- stats::approxfun(u, GY_inv, yleft=left-1e-5, yright=right+1e-5)
  y_grid <- seq(from=GY_inv[4], to=GY_inv[length(GY_inv)-3], length.out=500)
  
  # Get GY and gY
  GY_fn <- Vectorize(.inverse(GY_inv_fn, lower=min(u), upper=max(u)))
  dG_inv <- (GY_inv[3:length(GY_inv)] - GY_inv[1:(length(GY_inv)-2)])/(u[3:length(u)] - u[1:(length(u)-2)])
  dG_inv_fn <- stats::approxfun(0.5*(u[3:length(u)] + u[1:(length(u)-2)]), dG_inv, rule=2)
  gY_fn <- function(x){1/dG_inv_fn(GY_fn(x))}
  
  # Create SWIMw object
  if (all.equal(.gamma, function(x, alpha){as.numeric((x >= alpha) / (1 - alpha))})) {
    type <- rep("ES mean sd", length.out = max_length)
  } else {
    type <- list("RM mean sd")
  }
  
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
  
  # achieved mean and sd
  for(j in 1:max_length){
    mean_achieved <- .integrate(GY_inv, u)
    sd_achieved <- sqrt(.integrate((GY_inv - mean_achieved)^2, u))
    
    RM_achieved <-c()
    for (i in 1:length(alpha)){
      RM_achieved <- append(RM_achieved, .rm(GY_inv_fn(u), .gamma(u, alpha[i]), u))
    }
  }
  
  # Compare constraint and achieved stress
  f <- rep(list(function(x)x), length(k))
  m <- new_means
  z <- matrix(0, ncol = length(f), nrow = nrow(x_data))
  for (i in 1:length(f)){
    z[, i] <- apply(X = x_data[, k[[i]], drop = FALSE], MARGIN = 1, FUN = f[[i]])
  }
  min.fz <- apply(z, 2, min)
  max.fz <- apply(z, 2, max)
  if (any(m < min.fz) || any(m > max.fz)) stop("Values in m must be in the range of x")

  z <- cbind(1, z)
  moments <- function(x)colMeans(z * as.vector(exp(z %*% x))) - c(1, m)
  sol <- nleqslv::nleqslv(rep(0, length.out = length(f) + 1), moments, ...)
  if (sol$termcd != 1) warning(paste("nleqslv terminated with code ", sol$termcd))
  
  m.ac <- colMeans(z * as.vector(exp(z %*% sol$x)))[-1]
  err <- m - m.ac
  rel.err <- (err / m) * (m != 0)
  outcome <- data.frame(cols = as.character(k), required_mean = m, achieved_mean = m.ac, abs_error = err, rel_error = rel.err)
  print(outcome)  
  
  m <- new_sd
  m.ac <- sd_achieved
  err <- m - m.ac
  rel.err <- (err / m) * (m != 0)
  outcome <- data.frame(cols = as.character(k), required_sd = m, achieved_sd = m.ac, abs_error = err, rel_error = rel.err)
  print(outcome) 
  
  m <- q
  m.ac <- RM_achieved
  err <- m - m.ac
  rel.err <- (err / m) * (m != 0)
  outcome <- data.frame(cols = as.character(k), required_RM = m, achieved_RM = m.ac, abs_error = err, rel_error = rel.err)
  print(outcome) 
    
  # message if the achieved RM is different from the specified stress.
  if(any(q - RM_achieved > 1e-4)) {
    message(paste("Stressed RM specified was ", round(q, 4),", stressed RM achieved is ", round(RM_achieved, 4)))
    q <- RM_achieved
  }
  
  # message if the achieved mean or sd is different from the specified stress.
  if(mean_achieved - new_means > 1e-4) {
    message(paste("Stressed mean specified was ", round(new_means, 4),", stressed mean achieved is ", round(mean_achieved, 4)))
    new_means <- mean_achieved
  }
  if(sd_achieved - new_sd > 1e-4) {
    message(paste("Stressed sd specified was ", round(new_sd, 4),", stressed sd achieved is ", round(sd_achieved, 4)))
    new_sd <- sd_achieved
  }
  
  # Get constraints
  constr <- list(list("k"=k, "alpha"=alpha, "q"=q, "new_means"=new_means, "new_sd"=new_sd))
  names(constr) <- temp
  
  my_list <- SWIMw("x" = x_data, "u"=u, "h"=list(.h), "lam"=list(lam), "gamma" = list(.gamma),
                   "new_weights" = new_weights, "str_fY" = list(gY_fn), "str_FY" = list(GY_fn),
                   "str_FY_inv" = list(GY_inv_fn), "type" = type, "specs" = constr)
  
  if (is.SWIMw(x)) my_list <- merge(x, my_list)
  
  if (log) {
    summary_weights(my_list)
  }
  
  return(my_list)
}
