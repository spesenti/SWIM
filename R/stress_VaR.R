 #' Stressing Value-at-Risk
 #' 
 #' Provides the scenario weights such that the random variable
 #'    under the new scenraio weights fulfils the constraint on the VaR and
 #'    has minimal Kullback-Leibler divergence to the baseline random
 #'    variable.


 #' @param x       A SWIM object or a vector, matrix or data frame 
 #'     containing realisations of random variables. Colums of \code{x} 
 #'     are interpreted to correspond to random variables.
 #' @param k       Numeric, the column of \code{x} that is stressed
 #'     (default = 1).
 #' @param alpha   Numeric vector, the levels of the stressed VaR.
 #' @param q       Numeric vector, the stressed VaR at level \code{alpha}.
 #' @param q_ratio Numeric vector, the ratio of the stressed VaR to the
 #'      original VaR, \eqn{q_ratio =  q /  VaR}.

 #' @note If alpha and q or q_ratio are vectors, they have to be of the same length. 
 
 #' @details The new weights are 
 #' 
 #' @return A \code{\link{SWIM}} object containing \code{x}, the data; \code{new_weights}, a list of functions, that applied to the \code{k}th component of \code{x} generate the vectors of the new weights; and \code{specs}, the specification of what has been stressed.
 #'   The \code{specs} is a data.frame consisting of \code{type}, \code{k} \code{alpha} and \code{q}. Each row correponds to a different stress, see  \code{\link{SWIM}} object for details.
 #'       
 #'  
 #'    
 #' 
 #' @author Silvana M. Pesenti 
 #' 
 #' 
 #' @family stress functions
 #' @inherit stress seealso

 ## OUTPUT:  SWIM object with
 ## x              vector, matrix, data frame - realisations of a random variable
 ## new_weights    function - function that provides the weights if applied to the kth column of x
 ## k              numeric - column of x that are stressed (default = 1)
 ## alpha          numeric, vector - VaR level
 ## q              numeric, vector - constraints: new VaR at level alpha


 #' @export
 #' 
  stress_VaR <- function(x, alpha, q_ratio = NULL, q = NULL, k = 1){
   if(is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
   if(anyNA(x_data)) warning("'x' contains NA")
   if(any(alpha <= 0) || any(alpha >= 1)) stop("invalid 'alpha' argument")
   if(!is.null(q) && !is.null(q_ratio)) stop("only provide 'q' or 'q_ratio'")
   if(is.null(q) && is.null(q_ratio)) stop("no stress defined")

   ## x_data[, k] is the component of x_data that is stressed
   n <- length(x_data[, k])
   VaR <- quantile(x_data[, k], alpha, names = FALSE, type = 1)

   if(is.null(q)){
      if(!is.numeric(q_ratio)) stop("invalid 'q_ratio' argument")
      if(any(VaR == 0)) warning("VaR is 0, define 'q' instead of 'q_ratio'")
      if(length(alpha) > 1 && length(q_ratio) > 1 && length(alpha) != length(q_perc)) stop("arguments 'alpha' and 'q_ratio' must have length one or equal length")
      max_length <- max(length(q_ratio), length(alpha))
      q <- q_ratio * VaR
   }else{
      if(!is.numeric(q)) stop("invalid 'q' argument")
      if(length(alpha) > 1 && length(q) > 1 && length(alpha) != length(q)) stop("arguments 'alpha' and 'q' must have length one or equal length")
      max_length <- max(length(q), length(alpha))
   }  
   q <- rep(q, length.out = max_length)  
   alpha <- rep(alpha, length.out = max_length)

   ## check if VaR < q < ess sup (x_data)
   if(any(VaR > q)) print("VaR > q, quantile constraint is interpreted as probability constraint.")
   if(any(q > VaR & ecdf(x_data[, k])(VaR) == ecdf(x_data[, k])(q))) stop("There are not enough data points, specifically, there is none between VaR and q.")
   if(any(q >= max(x_data[, k])) || any(q <= min(x_data[, k]))) stop("all q need to be smaller than the largest data point and bigger than the smallest data point.") 

    # the temporary function 
    rn_VaR_temp <- function(y, constraints){
    constraints <- as.numeric(constraints)
    prob_q <- mean(y < constraints[2])
    rn_weights <- function(z)(constraints[1] / prob_q) * (z < constraints[2]) + (1 - constraints[1]) / (1 - prob_q) * (z >= constraints[2])
      return(rn_weights)
    }
    constr = cbind(alpha, q)
    new_weights <- apply(X = constr, FUN = rn_VaR_temp, MARGIN = 1, y = x_data[, k])
    if(is.null(colnames(x_data))) colnames(x_data) <-  paste("X", 1 : ncol(x_data), sep = "")
    names(new_weights) <- paste("stress", 1 : max_length)
    specs <- data.frame("type" = rep("VaR", length.out = max_length), "k" = rep(k, length.out = max_length), constr, stringsAsFactors = FALSE)
    rownames(specs) <- paste("stress", 1 : max_length)
    my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
   if(is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }

