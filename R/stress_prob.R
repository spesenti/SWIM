 ## DESCRIPTION:
 ## This function solves the optimisation problem: Given a random variable
 ## and a disjoint probabiltiy constraints, it provides the distribution of the random variable
 ## with is closest to the input wrt the Kullback-Leibler divergence and which fulfils the constraints.


 ## INPUT:
 ## x              vector, matrix, data frame - realisations of a random variable or SWIM object
 ##                By detfault, the first row of x is stressed
 ## k              numeric - column of x that are stressed (default = 1)
 ## prob           numeric, vector - probabilties of the intervals
 ## upper          numeric, vector - right endpoints of intervals
 ## lower          numeric, vector - left endpoints of intervals
 
 ## If alpha and q or q_perc are vectors, they have to be of the same length. 

 ## OUTPUT: SWIM object with
 ## x              vector, matrix, data frame - realisations of a random variable
 ## new_weights    function - function that provides the weights if applied to the kth column of x
 ## k              numeric - column of x that are stressed (default = 1)
 ## prob           numeric, vector - probabilties of the intervals
 ## upper          numeric, vector - ordered right endpoints of intervals
 ## lower          numeric, vector - ordered left endpoints of intervals

 ## If upper is not provided, the intervals are consequitive and prob is cumulativeiin the input only!!!

 ## ASSUMPTIONS:
 ## The data stem from continuously distributed random variables

  stress_prob <- function(x, prob, upper, lower= NULL, k = 1){
   if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
   if (anyNA(x_data)) warning("x contains NA")
   if (any(prob < 0) | any(prob > 1) | (is.null(lower) && sum(prob) > 1)) stop("Invalid prob argument")
   if (is.unsorted(upper)) stop("upper not sorted")
   if (!is.null(lower) && is.unsorted(lower)) stop("lower not sorted")
   if (!is.null(lower) && any(lower >= upper)) stop("lower has to be smaller than upper.")
   if ((length(upper) != length(prob)) | (!is.null(lower) && (length(lower) != length(prob)))) stop("Unequal length of lower, upper and prob")
   
   # if only one point is provided
   if (is.null(lower)){
     # probability of intervals
     prob_new <- c(prob, 1) - c(0, prob)
   } else { 
     prob_new <- c(prob, 1 - sum(prob))
   }
   if (is.null(lower)) lower <- c(min(x_data[,k]), upper[1:length(upper) - 1])
   
   # probabilty that P(a < x_data <= b) for constraints = (a,b)
   prob_old <- ecdf(x_data[,k])(upper) - ecdf(x_data[,k])(lower)
   prob_old <- c(prob_old, 1 - sum(prob_old))

   new_weights <- list(function(y) .rn_prob(y, constraints = cbind(lower, upper)) %*% as.matrix(prob_new / prob_old))
   if (is.null(colnames(x_data))) colnames(x_data) <-  paste("X", 1:ncol(x_data), sep = "")
    
   constr = cbind(lower, upper, prob)
   names(new_weights) <- paste("stress", 1)
   specs <- data.frame("type" = "prob", "k" = k, constr, stringsAsFactors = FALSE)
   rownames(specs) <- paste("stress", 1)
   my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
   if (is.SWIM(x)) my_list <- merge(x, my_list)
   return(my_list)  
  }

  .rn_prob <- function(y, constraints){
    .lower <- constraints[1]
    .upper <- constraints[2]
    # indicator function 1_{ a < x_data <= b}
    int <- function(limits, z) (limits[1] < z) * (z <= limits[2]) 
    interval <- cbind(apply(cbind(.lower, .upper), MARGIN = 1, FUN = int, z = y), 1 - rowSums(apply(cbind(.lower, .upper), MARGIN = 1, FUN = int, z = y)))
  return(interval)
  }  
  