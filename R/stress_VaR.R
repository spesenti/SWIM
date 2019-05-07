 ## DESCRIPTION:
 ## This function solves the optimisation problem: Given a random variable
 ## and a constraint on its VaR, it provides the distribution of the random variable
 ## with is closest to the input wrt the Kullback-Leibler divergence and which fulfils the constraints.


 ## INPUT:
 ## x              vector, matrix, data frame - realisations of a random variable or SWIM object
 ##                By detfault, the first row of x is stressed
 ## k              numeric - column of x that are stressed (default = 1)
 ## alpha          numeric, vector - level of VaR
 ## q              numeric, vector - constraints: new VaR at level alpha
 ## q_ratio        numeric, vector - constraints: q /  VaR

   ## If alpha and q or q_ratio are vectors, they have to be of the same length. 

 ## OUTPUT:  SWIM object with
 ## x              vector, matrix, data frame - realisations of a random variable
 ## new_weights    function - function that provides the weights if applied to the kth column of x
 ## k              numeric - column of x that are stressed (default = 1)
 ## alpha          numeric, vector - VaR level
 ## q              numeric, vector - constraints: new VaR at level alpha

 ## ASSUMPTIONS:
 ## The data stem from continuously distributed random variables

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

