 #' Stressing Value-at-Risk and Expected Shortfall
 #'
 #' This function solves the optimisation problem: Given a random variable
 #'   and a constraint on its VaR and ES, it provides the distribution of 
 #'   the random variable with is closest to the input wrt the Kullback-Leibler
 #'   divergence and fulfils the constraints.

 #' @inheritParams    stress_VaR
 #' @param s          Numeric, vector - constraints: new ES at level alpha
 #' @param s_ratio    Numeric, vector, ratio of stressed ES to base ES, \eqn{s_ratio = s / ES}.

 #' @inherit stress_VaR seealso
 
 #' @export
 #' 



## If q, s are vectors, they have to be of the same length. 
## If q is a vector and s numeric, the stress s is used for all q's. Similarly for s vector and q numeric.
## If alpha and q or s are vectors, they have to be of the same length.

## OUTPUT:
## x             vector, matrix, data.frame - realisation of a random variable
## new_weights   vector, matrix, data.frame - realisation of the Radon-Nikodym derivative
## k             numeric - column of x that are stressed (default = 1)
## alpha         level of VaR and ES 
## q             numeric, vector - constraints: new VaR at level alpha
## q_ratio       numeric, vector - constraints: q / VaR
## s             numeric, vector - constraints: new ES at level alpha
## s_ratio       numeric, vector - constraints: s /  ES

## ASSUMPTIONS:
## The realisations X stem from continuously distributed random variables

stress_VaR_ES <- function(x, alpha, q_ratio = NULL, s_ratio = NULL, q = NULL, s = NULL, k = 1){
  if(is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  if(anyNA(x_data)) warning("'x' contains NA")
  if(any(alpha <= 0) || any(alpha >= 1)) stop("invalid 'alpha' argument")
  if(!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
  if(!is.null(s) && !is.null(s_ratio)) stop("Only provide s or s_ratio")
  if(is.null(q) && is.null(q_ratio)) stop("no 'q' or 'q_ratio' defined")
  if(is.null(s) && is.null(s_ratio)) stop("no 's' or 's_ratio' defined")
  
  n <- length(x_data[, k])
  max_length <- max(length(alpha), length(q), length(q_ratio), length(s), length(s_ratio))
  
  ## VaR and ES of the input; type = 1 is the exact inverse of ecdf
  VaR <- quantile(x_data[, k], alpha, names = FALSE, type = 1)
  if(is.null(q)){
    if(!is.numeric(q_ratio)) stop("invalid 'q_ratio' argument")
    if(any(VaR == 0)) warning("VaR is 0, define 'q' instead if 'q_ratio'.")
    if(length(alpha) > 1 && length(q_ratio) > 1 && length(alpha) != length(q_perc)) stop("arguments 'alpha' and 'q_ratio' must have length one or equal length")
    q <- rep(q_ratio * VaR, length.out = max_length)
  }else{
    if(!is.numeric(q)) stop("invalid 'q' argument")
    if(length(alpha) > 1 && length(q) > 1 && length(alpha) != length(q)) stop("arguments 'alpha' and 'q' must have length one or equal length")
    q <- rep(q, length.out = max_length)  
  }  
  VaR_matrix <- matrix(rep(VaR, each = n), ncol = length(VaR))
  ES <- colMeans((x_data[, k] - VaR_matrix) * (x_data[, k] > VaR_matrix)) / (1 - alpha) + VaR
  
  if(is.null(s)){ 
    if(!is.numeric(s_ratio)) stop("invalid 's_ratio' argument")
    if(any(ES == 0)) warning("ES is 0, define 's' instead if 's_ratio'.")
    if(length(alpha) > 1 && length(s_ratio) > 1 && length(alpha) != length(s_ratio)) stop("arguments 'alpha' and 's_ratio' must have length one or equal length")
    s <- rep(s_ratio * ES, length.out = max_length)
  }else{
    if(!is.numeric(s)) stop("invalid 's' argument")
    if(length(alpha) > 1 && length(s) > 1 && length(alpha) != length(s)) stop("arguments 'alpha' and 's' must have length one or equal length")
    s <- rep(s, length.out = max_length)
  }
  alpha <- rep(alpha, length.out = max_length)
  
  ## check if the following constraints are fulfilled
  ## 1) Var < q, 2) q < s, 3) s < ess sup x_data, 4) E( x_data | x_data >= q ) < s
  ecdfx <- ecdf(x_data[, k])
  if(any(VaR > q)) print("VaR > q, quantile constraint is interpreted as probability constraint.")
  if(any(q > s)) stop("All q need to be smaller than s.")
  if(any(ecdfx(VaR) == ecdfx(q))) stop("There are not enough data points, specifically, there is none between VaR and q.")
  if(any(ecdfx(q) > ecdfx(s))) stop("There are not enough data points, specifically, there is none between q and s.")
  if(any(s >= max(x_data[, k])) || any(s <= min(x_data[, k]))) stop("all 's' need to be smaller than the largest data point and bigger than the smallest data point.") 
  
  q_matrix <- matrix(rep(q, each = n), ncol = max_length)
  if(any(colMeans(x_data[, k] * (x_data[, k] > q_matrix)) > s * (1 - ecdfx(q)))) stop("Expectation of X|X > q, needs to be smaller than the s.")
  
  # the temporary function 
  rn_VaR_ES_temp <- function(y, constraints){
    x_q <- 1 * (x_data[, k] >= constraints[2])
    ## function to calculate theta
    theta_sol <- function(theta){
      mean((x_data[, k] - constraints[3]) * exp(theta * (x_data[, k] - constraints[2])) * x_q)
    }
    theta <- uniroot(theta_sol, lower = 0, upper = 10^-20, tol = 10^-30, extendInt = "upX")$root
    prob_q <- mean(y < constraints[2])
    ## check existence of solution
    e <- mean(exp(theta * (x_data[, k] - constraints[2])) * (x_data[, k] >= constraints[2]))
    if(!all(constraints[1] * e <= prob_q * (1 - constraints[1]))){
      print("Weights are not unique.")}
    rn_weights <- function(z){(constraints[1] / prob_q) * (z < constraints[2]) + (1 - constraints[1]) / e * exp(theta * (z - constraints[2])) * (z >= constraints[2])}
    return(rn_weights)
  }
  
  constr <- cbind(alpha, q, s)
  new_weights <- apply(X = constr, FUN = rn_VaR_ES_temp, MARGIN = 1, y = x_data[, k])
  if(is.null(colnames(x_data))) colnames(x_data) <-  paste("X", as.character(1 : dim(x_data)[2]), sep = "")
  names(new_weights) <- paste(rep("stress", max_length), 1:max_length)
  specs <- data.frame("type" = rep("VaR ES", length.out = max_length), "k" = rep(k, length.out = max_length), constr, stringsAsFactors = FALSE)
  rownames(specs) <- paste(rep("stress", max_length), 1 : max_length)
  my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
  if(is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
}