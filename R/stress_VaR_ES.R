 #' Stressing Value-at-Risk and Expected Shortfall
 #'
 #' Provides scenario weights such that the random variable
 #'    under the new scenraio weights fulfils the constraint on the VaR and
 #'    ES has minimal Kullback-Leibler divergence to the baseline random
 #'    variable.

 #' @inheritParams    stress_VaR
 #' @param s          Numeric, vector - constraints: new ES at level alpha.
 #' @param s_ratio    Numeric, vector, ratio of stressed ES to base ES,
 #'                   \eqn{s_ratio = s / ES}.
 #' 
 #' @details The new weights are 
 #'     If q, s are vectors, they have to be of the same length.
 #'     If q is a vector and s numeric, the stress s is used for all q's. Similarly for s vector and q numeric.
 #'     If alpha and q or s are vectors, they have to be of the same length.
 #' 
 #' @return A \code{SWIM} object containing:
 #'     \itemize{
 #'       \item \code{x}, the data;
 #'       \item \code{new_weights}, a list of functions, that applied to
 #'       the \code{k}th colum of \code{x} generate the vectors of 
 #'       scenario weights;
 #'       \item \code{specs}, the specification of what has been
 #'       stressed.
 #'       The \code{specs} is a data.frame consisting of \code{type},
 #'       \code{k}, \code{alpha}, \code{q} and \code{s}. Each row 
 #'       correponds to a differentstress, see \code{\link{SWIM}} object
 #'       for details.
 #'     }
 #'     
 #' @family stress functions 
 #' @export

stress_VaR_ES <- function(x, alpha, q_ratio = NULL, 
  s_ratio = NULL, q = NULL, s = NULL, k = 1){
  
  if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if (!is.null(s) && !is.null(s_ratio)) stop("Only provide s or s_ratio")
  if (is.null(q) && is.null(q_ratio)) stop("no q or q_ratio defined")
  if (is.null(s) && is.null(s_ratio)) stop("no s or s_ratio defined")
  
  n <- length(x_data[, k])
  max_length <- max(length(alpha), length(q), length(q_ratio), length(s), length(s_ratio))
  
  # type = 1 in quantile is the exact inverse of ecdf
  VaR <- quantile(x_data[, k], alpha, names = FALSE, type = 1)
  if (is.null(q)){
    if (!is.numeric(q_ratio)) stop("Invalid q_ratio argument")
    if (any(VaR == 0)) warning("VaR is 0, define q instead if q_ratio.")
    if (length(alpha) > 1 && length(q_ratio) > 1 && length(alpha) != length(q_perc)) stop("Arguments alpha and q_ratio must have length one or equal length.")
    q <- rep(q_ratio * VaR, length.out = max_length)
  } else {
    if (!is.numeric(q)) stop("invalid q argument")
    if (length(alpha) > 1 && length(q) > 1 && length(alpha) != length(q)) stop("arguments alpha and q must have length one or equal length")
    q <- rep(q, length.out = max_length)  
  }  
  VaR_matrix <- matrix(rep(VaR, each = n), ncol = length(VaR))
  ES <- colMeans((x_data[, k] - VaR_matrix) * (x_data[, k] > VaR_matrix)) / (1 - alpha) + VaR
  
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
  ## 1) Var < q, 2) q < s, 3) s < ess sup x, 4) E( x | x >= q ) < s
  ecdfx <- ecdf(x_data[, k])
  if (any(VaR > q)) print("VaR > q, quantile constraint is interpreted as probability constraint.")
  if (any(q > s)) stop("All q need to be smaller than s.")
  if (any(ecdfx(VaR) == ecdfx(q))) stop("There are not enough data points, specifically, there is none between VaR and q.")
  if (any(ecdfx(q) > ecdfx(s))) stop("There are not enough data points, specifically, there is none between q and s.")
  if (any(s >= max(x_data[, k])) || any(s <= min(x_data[, k]))) stop("all s need to be smaller than the largest and larger than the smallest data point.") 
  
  q_matrix <- matrix(rep(q, each = n), ncol = max_length)
  if (any(colMeans(x_data[, k] * (x_data[, k] > q_matrix)) > s * (1 - ecdfx(q)))) stop("Expectation of X|X > q, needs to be smaller than the s.")
  
  constr <- cbind(alpha, q, s)
  new_weights <- apply(X = constr, MARGIN = 1, FUN = .rn_VaR_ES, y = x_data[, k])
  if (is.null(colnames(x_data))) colnames(x_data) <-  paste("X", as.character(1:dim(x_data)[2]), sep = "")
  names(new_weights) <- paste(rep("stress", max_length), 1:max_length)
  specs <- data.frame("type" = rep("VaR ES", length.out = max_length), "k" = rep(k, length.out = max_length), constr, stringsAsFactors = FALSE)
  rownames(specs) <- paste(rep("stress", max_length), 1:max_length)
  my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
  if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
}

 # help function 
  .rn_VaR_ES <- function(y, constraints){
    .alpha <- constraints[1]
    .q <- constraints[2]
    .s <- constraints[3]
    x_q <- 1 * (y >= .q)

    theta_sol <- function(theta){
      mean((y - .s) * exp(theta * (y - .q)) * x_q)
    }
    
    theta <- uniroot(theta_sol, lower = 0, upper = 10^-20, tol = 10^-30, extendInt = "upX")$root
    prob_q <- mean(y < .q)
    # check existence of solution
    e <- mean(exp(theta * (y - .q)) * (y >= .q))
    if (!all(.alpha * e <= prob_q * (1 - .alpha))) print("Weights are not unique.")
    
    rn_weights <- function(z){(.alpha / prob_q) * (z < .q) + (1 - .alpha) / e * exp(theta * (z - .q)) * (z >= .q)}
  return(rn_weights)
  }