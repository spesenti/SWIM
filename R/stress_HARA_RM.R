#' Stressing Risk Measure and HARA Utility
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfills a
#'     constraint on its HARA utility defined by \code{a}, \code{b} and 
#'     \code{eta} parameter and risk measure defined by a \code{gamma} 
#'     function and evaluated at a given level \code{alpha}. Scenario weights are
#'     selected by constrained minimisation of the Wasserstein distance to the
#'     baseline model.
#' @inheritParams    stress_RM_w
#' @param k       Numeric, the column of \code{x} that is stressed
#'     \code{(default = 1)}.\cr
#' @param a   Numeric vector, input to HARA utility function.\cr
#' @param b   Numeric vector, input to HARA utility function.\cr
#' @param eta   Numeric vector, input to HARA utility function.\cr
#' @param q          Numeric, vector, the stressed RM at level
#'                   \code{alpha} (must be same length as \code{alpha}).\cr
#' @param q_ratio    Numeric, vector, the ratio of the stressed RM to
#'                   the baseline RM (must be same length as \code{alpha}).\cr
#' @param hu          Numeric, vector, the stressed HARA utility with parameters
#' \code{a}, \code{b} and \code{eta}.\cr
#' @param hu_ratio    Numeric, vector, the ratio of the HARA utility to the 
#' baseline HARA utility.\cr
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
#'     The HARA Utility is defined by 
#'     \deqn{u(x) = \frac{1-eta}{eta}(\frac{ax}{1 - eta} + b)^eta}.
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
#'      \item \code{type = "HARA RM"};
#'      \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}, \code{alpha}, \code{a}, \code{b},
#'    \code{eta}, \code{q}, and \code{hu}.
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
#' res1 <- stress_wass(type = "HARA RM", x = x, a=1, b=5, eta=0.5, alpha=0.95,
#'  q_ratio=1.05, hu_ratio=1.05, k=1)
#'   summary(res1)
#'
#' ## calling stress_RM_w directly
#' ## stressing "gamma"
#' res2 <- stress_HARA_RM_w(x = x, a=1, b=5, eta=0.5, alpha=0.95,
#'  q_ratio=1.05, hu_ratio=1.05, k=2)
#' summary(res2)
#'}
#'
#' @family stress functions
#' @references \insertRef{Pesenti2019reverse}{SWIM}\cr
#'
#'     \insertRef{Pesenti2020SSRN}{SWIM}\cr
#'
#'     \insertRef{Pesenti2021SSRN}{SWIM}
#' @export


stress_HARA_RM_w <- function(x, alpha = 0.8, a, b, eta, 
                        q_ratio = NULL, q = NULL, hu_ratio = NULL, hu=NULL,
                        k = 1, h = 1, gamma = NULL, names = NULL, log = FALSE, method = "Nelder-Mead"){

  if (is.SWIM(x) | is.SWIMw(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide one of q or q_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("No q or q_ratio defined")
  if (!is.null(hu) && !is.null(hu_ratio)) stop("Only provide one of hu or hu_ratio")
  if (is.null(hu) && is.null(hu_ratio)) stop("No hu or hu_ratio defined")

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

  # Calculate the risk measure and HARA utility
  if(is.null(q)){
    q = c()
    for (i in 1:length(q_ratio)){
      q <- append(q, .rm(FY_inv_fn(u), .gamma(u, alpha[i]), u) * q_ratio[i])
    }
  }
  if(is.null(hu)){hu <- .hara_utility(a, b, eta, u, FY_inv_fn(u)) * hu_ratio}
  
  .objective_fn <- function(par){
    # Get ell = F_inv + sum(lam*gamma)
    ell_fn <- function(x){
      ell <- FY_inv_fn(x)
      for (i in 1:(length(par)-1)){
        ell <- ell + par[i+1]*.gamma(x, alpha[i])
      }
      return(ell)
    }
    # Get isotonic projection of ell
    iso_g <- stats::isoreg(u, ell_fn(u))$yf
    
    # Get Utransform
    GY_inv <- .utransform(a, b, eta, u, iso_g, exp(par[1]), 600)

    rm_stress <-c ()
    for (i in 1:length(alpha)){
      rm_stress <- append(rm_stress, .rm(GY_inv, .gamma(u, alpha[i]), u))
    }
    hara_stress <- .hara_utility(a, b, eta, u, GY_inv)
    
    # Return RM error
    return(sqrt(sum(q - rm_stress)^2 + (hara_stress - hu)^2))
  }
  
  # Run optimization
  # May require loop to have random reinitializations of init_lam if result of 
  # optim is not good enough
  print("Run optimization")
  max_length <- length(q)
  init_lam <- stats::rnorm(1+max_length)
  res <- stats::optim(init_lam, .objective_fn, method = method)
  lam <- res$par
  print("Optimization converged")
  
  # Get ell
  ell_fn <- function(x){
    ell <- FY_inv_fn(x)
    for (i in 1:(length(lam)-1)){
      ell <- ell + lam[i+1]*.gamma(x, alpha[i])
    }
    return(ell)
  }
  ell <- ell_fn(u)

  print("Calculate optimal scenario weights")
  # Get GY_inv, y_grid
  iso_g <- stats::isoreg(u, ell)$yf
  GY_inv <- .utransform(a, b, eta, u, iso_g, exp(lam[1]), 600)
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
    type <- rep("HARA ES", length.out = max_length)
  } else {
    type <- list("HARA RM")
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
  
  # achieved RM
  for(j in 1:max_length){
    RM_achieved <-c()
    for (i in 1:length(alpha)){
      RM_achieved <- append(RM_achieved, .rm(GY_inv_fn(u), .gamma(u, alpha[i]), u))
    }
  }
  
  # Compare constraint and achieved stress
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
  
  # Compare constraint and achieved stress
  hara_achieved <- .hara_utility(a, b, eta, u, GY_inv)
  m <- hu
  m.ac <- hara_achieved
  err <- m - m.ac
  rel.err <- (err / m) * (m != 0)
  outcome <- data.frame(cols = as.character(k), required_RM = m, achieved_moment = m.ac, abs_error = err, rel_error = rel.err)
  print(outcome)  
  
  # message if the achieved HARA utility is different from the specified utility.
  if(hu - hara_achieved > 1e-4) {
    message(paste("Stressed HARA Utility specified was ", round(hu, 4),", stressed HARA Utility achieved is ", round(hara_achieved, 4)))
    hu <- hara_achieved
  }
  
  # Get constraints
  constr <- list(list("k"=k, "alpha"=alpha, "q"=q, "hu"=hu, "a"=a, "b"=b, "eta"=eta))
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
