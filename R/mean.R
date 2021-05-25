#' Mean of a Stressed Model
#' 
#' Provides the mean of a stressed model component (random variable) under the scenario weights. 
#' 
#' @inheritParams cdf 
#' 
#' @return The mean of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#' 
#' @details \code{mean_stressed}: The mean of a chosen stressed model component, subject to the calculated scenario weights.
#'      The mean of a stressed model component is denoted as \deqn{\overline{X^W}}
#'
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed mean
#' mean_stressed(res1, xCol = 1, wCol = 1)
#' ## baseline mean
#' mean(x$normal)
#' 
#' @author Kent Wu
#' @describeIn mean_stressed mean of stressed model components
#' 
#' @seealso See \code{\link{stress_moment}} stressing a baseline 
#'     model with desired moment constraints, and \code{\link{sd}} computes
#'     stressed standard deviations under the scenario weights
#' @export

mean_stressed <- function(object, xCol = 1, wCol = 1){
  if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  new_weights <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  mean <- .mean(x = x_data, w = new_weights)
  return(mean)
}

# tst 

# help function 
# x    numeric vector  
# w    numeric vector with weights
.mean <- function(x, w){
  n <- length(as.vector(x))
  mean_w <- stats::weighted.mean(x = x, w = w)
  return(mean_w)
}
