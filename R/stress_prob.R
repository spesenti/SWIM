#' Stressing Intervals
#' 
#' Provides scenario weights such that the random variable
#'    under the scenraio weights fulfils the constraints on the 
#'    probability sets and has minimal Kullback-Leibler divergence to 
#'    the baseline random variable.
#'    
#' @inheritParams stress_VaR 
#' @param lower   Numeric vector, left endpoints of the intervals 
#'                (default = NULL).
#' @param upper   Numeric vector, right endpoints of the intervals.
#' @param prob    Numeric vector, stressed probabilties corresponding to
#'                the intervals defined through \code{lower} and 
#'                \code{upper}.
#'      
#' @details If \code{upper = NULL}, the intervals are consequitive and 
#'     \code{prob} cummulative.
#' 
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a list of functions, that applied to the
#'     \code{k}th colum of \code{x} generate the vectors of scenario
#'     weights;
#'     \item \code{specs}, the specification of what has been
#'     stressed.
#'     The \code{specs} is a data.frame consisting of \code{type}, \code{k},
#'     \code{lower}, \code{upper} and \code{prob}. Each row correponds to a different 
#'     stress, see \code{\link{SWIM}} for details.
#'     }
#'     
#' @author Silvana M. Pesenti 
#' 
#' @family stress functions 
#' @inherit SWIM references 
#' @export
#' 

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
  