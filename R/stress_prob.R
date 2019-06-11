#' Stressing Intervals
#' 
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that a stressed model component (random variable) 
#'     fulfils constraints on probability of disjoint intervals. Scenario 
#'     weights are selected by constrained minimisation of the 
#'     relative entropy to the baseline model.
#'    
#' @inheritParams stress_VaR 
#' @param lower   Numeric vector, left endpoints of the intervals. 
#' @param upper   Numeric vector, right endpoints of the intervals.
#' @param prob    Numeric vector, stressed probabilities corresponding to
#'                the intervals defined through \code{lower} and 
#'                \code{upper}.
#'      
#' @details The intervals are treated as half open intervals, that is
#'     the lower endpoint are not included, whereas the upper endpoint 
#'     are included. If \code{upper = NULL}, the intervals 
#'     are consecutive and \code{prob} cumulative.\cr
#'     The intervals defined through \code{lower} and \code{upper} must
#'     be disjoint. 
#' 
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list of functions, that applied to the
#'     \code{k}th column of \code{x}, generate the vectors of scenario
#'     weights. Each component corresponds to a different stress;
#'     \item \code{type = "prob"};
#'     \item \code{specs}, a list, each component corresponds to 
#'     a different stress and contains \code{k}, \code{lower},  
#'     \code{upper} and \code{prob}. 
#'     }
#'     See \code{\link{SWIM}} for details.
#'     
#' @author Silvana M. Pesenti 
#' 
#' @examples 
#' set.seed(0)
#' x <- rnorm(1000)
#' ## consecutive intervals
#' res1 <- stress(type = "prob", x = x, prob = 0.008, upper = -2.4)
#' # probability under the stressed model
#' cdf(res1, xCol = 1)(-2.4)
#' 
#' ## calling stress_prob directly
#' ## multiple intervals
#' res2 <- stress_prob(x = x, prob = c(0.008, 0.06), 
#'   upper = c(-2.4, -1.6), lower = c(-3, -2))
#' # probability under the stressed model
#' cdf(res2, xCol = 1)(c(-2.4, -1.6)) - cdf(res2, xCol = 1)(c(-3, -2))
#' 
#' @family stress functions 
#' @inherit SWIM references 
#' @export
#' 

  stress_prob <- function(x, prob, lower= NULL, upper, k = 1){
   if (is.SWIM(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
   if (anyNA(x_data)) warning("x contains NA")
   if (any(prob < 0) | any(prob > 1) | (is.null(lower) && sum(prob) > 1)) stop("Invalid prob argument")
   if (is.unsorted(upper)) stop("upper not sorted")
   if (!is.null(lower) && is.unsorted(lower)) stop("lower not sorted")
   if (!is.null(lower) && any(lower >= upper)) stop("lower has to be smaller than upper.")
   if (!is.null(lower) && any(lower[-1] < upper[-length(upper)])) stop("Intervals not disjoint")
   if (!is.null(lower) && lower[1] < min(x_data[,k])) stop("Interval not in the range of x")
   if (upper[length(upper)] > max(x_data[,k])) stop("Interval not in the range of x")
   if ((length(upper) != length(prob)) | (!is.null(lower) && (length(lower) != length(prob)))) stop("Unequal length of lower, upper and prob")
   
   # if only one point is provided
   if (is.null(lower)){
     # probability of intervals
     prob_new <- c(prob, 1) - c(0, prob)
     lower <- c(min(x_data[,k]), upper[-length(upper)])
     prob_old <- stats::ecdf(x_data[,k])(upper)
   } else { 
     prob_new <- c(prob, 1 - sum(prob))
     prob_old <- stats::ecdf(x_data[,k])(upper) - stats::ecdf(x_data[,k])(lower)
   }

   # probabilty that P(a < x_data <= b) for constraints = (a,b)
   prob_old <- c(prob_old, 1 - sum(prob_old))
   if (any(prob_old == 0)) stop("Not enough data points.")
   new_weights <- list(function(y) .rn_prob(y, constraints = cbind(lower, upper)) %*% as.matrix(prob_new / prob_old))
   if (is.null(colnames(x_data))) colnames(x_data) <-  paste("X", 1:ncol(x_data), sep = "")
   names(new_weights) <- paste("stress", 1)
   
   constr_prob <- list("k" = k, "prob" = prob, "upper" = upper, "lower" = lower)
   constr <- list(constr_prob)
   names(constr) <- paste("stress", 1)
   type <- list("prob")
   my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "type" = type, "specs" = constr)
   if (is.SWIM(x)) my_list <- merge(x, my_list)
   return(my_list)  
  }

  .rn_prob <- function(y, constraints){
    # indicator function 1_{ a < x_data <= b}

    int1 <- function(limits, z) 1 * (z <= limits[2])
    int <- function(limits, z) (limits[1] < z) * (z <= limits[2]) 
    nconst <- nrow(constraints)
   if (nconst > 1){
  # more than 2 intervals
   f_first <- function(z) int1(limits = constraints[1,], z = z)
   f_mid <- function(z) apply(matrix(constraints[2:nconst,], ncol = 2), MARGIN = 1, FUN = int, z = z)
   interval <- cbind(f_first(y), f_mid(y), 1 - rowSums(cbind(f_first(y), f_mid(y))))
    } else {
  # only one interval     
    interval <- cbind(apply(constraints, MARGIN = 1, FUN = int1, z = y), 1 - rowSums(apply(constraints, MARGIN = 1, FUN = int1, z = y)))
     }
  return(interval)
  }  