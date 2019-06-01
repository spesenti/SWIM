 #' Stressing Value-at-Risk
 #' 
 #' Provides weights on simulated scenarios from a baseline stochastic
 #'     model, such that a stressed model component (random variable) fulfils a 
 #'     constraint on its quantile at a given level, also known as 
 #'     Value-at-Risk (VaR). Scenario weights are selected by 
 #'     constrained minimisation of the relative entropy to the 
 #'     baseline model.
 #'    
 #' @param x       A vector, matrix or data frame 
 #'     containing realisations of random variables. Columns of \code{x} 
 #'     correspond to random variables; OR\cr
 #'     A \code{SWIM} object, where \code{x} corresponds to the 
 #'     underlying data of the \code{SWIM} object.
 #' @param k       Numeric, the column of \code{x} that is stressed
 #'     \code{(default = 1)}.
 #' @param alpha   Numeric vector, the levels of the stressed VaR.
 #' @param q       Numeric vector, the stressed VaR at level 
 #'                \code{alpha}.\cr
 #'                If \code{alpha} and \code{q} are vectors, they must 
 #'                have the same length. 
 #' @param q_ratio Numeric vector, the ratio of the stressed VaR to the
 #'                baseline VaR.\cr
 #'                If \code{alpha} and \code{q_ratio} are vectors, they 
 #'                must have the same length. 
 #'      
 #' @details The stressed VaR is the quantile of the chosen model component,
 #'      subject to the calculated scenario weights. 
 #'      The VaR at level \code{alpha} of a random variable with distribution 
 #'      function F is defined as its left-quantile at alpha:
 #'      \deqn{VaR_alpha = F^{-1}(alpha).} 
 #'      
 #' If one of \code{alpha, q} (\code{q_ratio}) is a vector,
 #'    the stressed VaR's of the \code{k}th column of \code{x}, at levels 
 #'    \code{alpha}, are equal to \code{q}.
 #'     
 #' @return A \code{SWIM} object containing:
 #'     \itemize{
 #'       \item \code{x}, the data;
 #'       \item \code{new_weights}, a list of functions, that, applied to the
 #'     \code{k}th column of \code{x} generate the vectors of scenario
 #'     weights;
 #'      \item \code{specs}, the specification of what has been
 #'     stressed.
 #'     \code{specs} is a data.frame consisting of \code{type}, \code{k},
 #'     \code{alpha} and \code{q}. Each row corresponds to a different 
 #'     stress.
 #'     }
 #'     See \code{\link{SWIM}} for details.
 #' @author Silvana M. Pesenti 
 #' 
 #' @examples 
 #' set.seed(0)
 #' x <- as.data.frame(cbind(
 #'   "normal" = rnorm(1000), 
 #'   "gamma" = rgamma(1000, shape = 2)))
 #' res1 <- stress(type = "VaR", x = x, 
 #'   alpha = 0.9, q_ratio = 1.05)
 #'   
 #' ## calling stress_VaR directly
 #' ## stressing "gamma" 
 #' res2 <- stress_VaR(x = x, alpha = 0.9, 
 #'   q_ratio = c(1.03, 1.05), k = 2)
 #' get.specs(res2)
 #' summary(res2)
 #'    
 #' @family stress functions 
 #' @inherit SWIM references 
 #' @export
 #' 
  stress_VaR <- function(x, alpha, q_ratio = NULL, q = NULL, k = 1){
   if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
   if (anyNA(x_data)) warning("x contains NA")
   if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
   if (!is.null(q) && !is.null(q_ratio)) stop("Only provide q or q_ratio")
   if (is.null(q) && is.null(q_ratio)) stop("No stress defined")

   ## x_data[, k] component of x_data that is stressed
   n <- length(x_data[, k])
   VaR <- stats::quantile(x_data[, k], alpha, names = FALSE, type = 1)

   if(is.null(q)){
      if (!is.numeric(q_ratio)) stop("Invalid q_ratio argument")
      if (any(VaR == 0)) warning("VaR is 0, define q instead of q_ratio.")
      if (length(alpha) > 1 && length(q_ratio) > 1 && length(alpha) != length(q_ratio)) stop("Arguments alpha and q_ratio must have length one or equal length.")
      max_length <- max(length(q_ratio), length(alpha))
      q <- q_ratio * VaR
   } else {
      if (!is.numeric(q)) stop("Invalid q argument")
      if (length(alpha) > 1 && length(q) > 1 && length(alpha) != length(q)) stop("Arguments alpha and q must have length one or equal length.")
      max_length <- max(length(q), length(alpha))
   }  
   q <- rep(q, length.out = max_length)  
   alpha <- rep(alpha, length.out = max_length)

   ## check if VaR < q < ess sup (x_data)
   if (any(VaR > q)) print("VaR > q, quantile constraint interpreted as probability constraint.")
   if (any(q > VaR & stats::ecdf(x_data[, k])(VaR) == stats::ecdf(x_data[, k])(q))) stop("There are not enough data points, specifically, there is none between VaR and q.")
   if (any(q >= max(x_data[, k])) || any(q <= min(x_data[, k]))) stop("All q need to be smaller than the largest and larger than the smallest data point.") 

    constr <- cbind(alpha, q)
    new_weights <- apply(X = constr, MARGIN = 1, FUN = .rn_VaR, y = x_data[, k])
    if (is.null(colnames(x_data))) colnames(x_data) <-  paste("X", 1:ncol(x_data), sep = "")
    names(new_weights) <- paste("stress", 1:max_length)

    type <- rep(list("VaR"), length.out = max_length)
    constr1 <- cbind("k" = rep(k, length.out = max_length), constr)
    constr_VaR <- list()
    for(s in 1:max_length){
      temp_list <- list(as.list(constr1[s, ]))
      names(temp_list) <- paste("stress", s)
      constr_VaR <- c(constr_VaR, temp_list)
    }
    my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "type" = type, "specs" = constr_VaR)
   if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }
  
  # help function 
  .rn_VaR <- function(y, constraints){
     .alpha <- as.numeric(constraints[1])
     .q <- as.numeric(constraints[2])
     prob_q <- mean(y < .q)
     rn_weights <- function(z)(.alpha / prob_q) * (z < .q) + (1 - .alpha) / (1 - prob_q) * (z >= .q)
     return(rn_weights)
  }