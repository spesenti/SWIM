#' Stressing Value-at-Risk and Expected Shortfall
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) fulfils a 
#'     constraint on its Value-at-Risk (VaR) and Expected Shortfall (ES) risk 
#'     measures, both evaluated at a given level. Scenario weights are 
#'     selected by constrained minimisation of the relative entropy to the 
#'     baseline model.
#'     
#' @inheritParams    stress_VaR
#' @param s          Numeric, vector, the stressed ES at level 
#'                   \code{alpha}.\cr
#'                   If \code{q} and \code{s} are vectors, they must have
#'                   the same length.
#' @param s_ratio    Numeric, vector, the ratio of the stressed ES to 
#'                   the baseline ES.\cr
#'                   If \code{q} (\code{q_ratio}) and \code{s_ratio} are vectors, 
#'                   they must have the same length.
#' 
#' @details The VaR at level \code{alpha} of a random variable with 
#'     distribution function F is defined as its left-quantile at \code{alpha}:
#'     \deqn{VaR_alpha = F^{-1}(alpha).}
#'     
#'     The ES at level \code{alpha} of a random variable with distribution 
#'     function F is defined by:
#'     \deqn{ES_alpha = 1 / (1 - alpha) * int_alpha^1 VaR_u d u.}
#' 
#'     The stressed VaR and ES are the risk measures of the chosen model 
#'     component, subject to the calculated scenario weights. If one 
#'     of \code{alpha, q, s} (\code{q_ratio, s_ratio}) is 
#'     a vector, the stressed VaR's and ES's of the \code{k}th column of  
#'     \code{x}, at levels \code{alpha}, are equal to \code{q} 
#'     and \code{s}, respectively. 
#'
#'    The stressed VaR specified, either via \code{q} or \code{q_ratio}, might not equal
#'    the attained empirical VaR of the model component. In this 
#'    case, \code{stress_VaR} will display a \code{message} and the \code{specs} contain
#'    the achieved VaR. Further, ES is then calculated on the bases of the achieved VaR.
#' 
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list of functions, that applied to 
#'   the \code{k}th column of \code{x}, generates the vectors of scenario 
#'   weights. Each component corresponds to a different stress;
#'      \item \code{type = "VaR ES"};
#'      \item \code{specs}, a list, each component corresponds to 
#'    a different stress and contains \code{k}, \code{alpha}, 
#'    \code{q} and \code{s}.
#'     }
#'     See \code{\link{SWIM}} for details.
#' 
#' @examples 
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR ES", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05, s_ratio = 1.08)
#'   
#' ## calling stress_VaR_ES directly   
#' ## stressing "gamma"
#' res2 <- stress_VaR_ES(x = x, alpha = 0.9, 
#'   q_ratio = 1.03, s_ratio = c(1.05, 1.08), k = 2)
#' get_specs(res2)
#' summary(res2)
#'             
#' @family stress functions 
#' @inherit SWIM references 
#' @export

stress_VaR_ES <- function(x, alpha, q_ratio = NULL, 
                          s_ratio = NULL, q = NULL, s = NULL, k = 1){
  
  if (is.SWIM(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if (!is.null(s) && !is.null(s_ratio)) stop("Only provide s or s_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("no q or q_ratio defined")
  if (is.null(s) && is.null(s_ratio)) stop("no s or s_ratio defined")
  
  n <- length(x_data[, k])
  max_length <- max(length(alpha), length(q), length(q_ratio), length(s), length(s_ratio))
  
  # type = 1 in quantile is the exact inverse of ecdf
  VaR <- stats::quantile(x_data[, k], alpha, names = FALSE, type = 1)
  if (is.null(q)){
    if (!is.numeric(q_ratio)) stop("Invalid q_ratio argument")
    if (any(VaR == 0)) warning("VaR is 0, define q instead if q_ratio.")
    if (length(alpha) > 1 && length(q_ratio) > 1 && length(alpha) != length(q_ratio)) stop("Arguments alpha and q_ratio must have length one or equal length.")
    q <- rep(q_ratio * VaR, length.out = max_length)
  } else {
    if (!is.numeric(q)) stop("invalid q argument")
    if (length(alpha) > 1 && length(q) > 1 && length(alpha) != length(q)) stop("arguments alpha and q must have length one or equal length")
    q <- rep(q, length.out = max_length)  
  }  
  # VaR achieved is the largest simulated value that is less or equal to q
  VaR_achieved <- vector('numeric', length = max_length)
  for (i in 1:max_length) {
    VaR_achieved[i] <- max(x_data[, k][x_data[, k] <= q[i]])
    # message if the achieved VaR is different from the specified stress.
    if(q[i] != VaR_achieved[i])message(paste("Stressed VaR specified was", round(q[i], 4),", stressed VaR achieved is", round(VaR_achieved[i], 4)))
  }
  
  VaR_matrix <- matrix(rep(VaR_achieved, each = n), ncol = length(VaR_achieved))
  ES <- colMeans((x_data[, k] - VaR_matrix) * (x_data[, k] > VaR_matrix)) / (1 - alpha) + VaR_achieved
  
  if (is.null(s)){ 
    if (!is.numeric(s_ratio)) stop("Invalid s_ratio argument")
    if (any(ES == 0)) warning("ES is 0, define s instead if s_ratio.")
    if (length(alpha) > 1 && length(s_ratio) > 1 && length(alpha) != length(s_ratio)) stop("Arguments alpha and s_ratio must have length one or equal length.")
    s <- rep(s_ratio * ES, length.out = max_length)
  } else {
    if (!is.numeric(s)) stop("Invalid s argument")
    if (length(alpha) > 1 && length(s) > 1 && length(alpha) != length(s)) stop("Arguments alpha and s must have length one or equal length.")
    s <- rep(s, length.out = max_length)
  }
  alpha <- rep(alpha, length.out = max_length)
  
  ## check if the following constraints are fulfilled
  ## 1) Var < q, 2) q < s, 3) s < ess sup x
  ecdfx <- stats::ecdf(x_data[, k])
  if (any(q > s)) stop("All q need to be smaller than s.")
  if (any(VaR != q & ecdfx(VaR) == ecdfx(q))) stop("There are not enough data points, specifically, there is none between VaR and q.")
  if (any(abs(ecdfx(q) - ecdfx(s)) <= 1/n)) stop("There are not enough data points, specifically, there is none between q and s.")
  if (any(s >= max(x_data[, k])) || any(q <= min(x_data[, k]))) stop("all s need to be smaller than the largest and all q larger than the smallest data point.") 
  
  q_matrix <- matrix(rep(VaR_achieved, each = n), ncol = max_length)

  constr <- cbind(alpha, "q"= VaR_achieved, s)
  new_weights <- apply(X = constr, MARGIN = 1, FUN = .rn_VaR_ES, y = x_data[, k])
  if (is.null(colnames(x_data))) colnames(x_data) <-  paste("X", as.character(1:dim(x_data)[2]), sep = "")
  names(new_weights) <- paste(rep("stress", max_length), 1:max_length)
  
  type <- rep(list("VaR ES"), length.out = max_length)
  constr1 <- cbind("k" = rep(k, length.out = max_length), constr)
  constr_ES <- list()
  for(s in 1:max_length){
    temp_list <- list(as.list(constr1[s, ]))
    names(temp_list) <- paste("stress", s)
    constr_ES <- c(constr_ES, temp_list)
  }
  my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "type" = type, "specs" = constr_ES)
  if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
}

# help function 
.rn_VaR_ES <- function(y, constraints){
  .alpha <- constraints[1]
  .q <- constraints[2]
  .s <- constraints[3]
  x_q <- 1 * (y > .q)
  
  theta_sol <- function(theta){
    mean((y - .s) * exp(theta * (y - .q)) * x_q)
  }
  
  theta <- stats::uniroot(theta_sol, lower = -10^-20, upper = 10^-20, tol = 10^-30, extendInt = "yes")$root
  prob_q <- mean(y <= .q)
  e <- mean(exp(theta * (y - .q)) * (y > .q))
  rn_weights <- function(z){(.alpha / prob_q) * (z <= .q) + (1 - .alpha) / e * exp(theta * (z - .q)) * (z > .q)}
  return(rn_weights)
}