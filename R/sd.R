#' Standard deviation and variance of a Stressed Model
#' 
#' Provides the standard deviation and variance of a stressed 
#'     model component (random variable) under the scenario weights. 
#' 
#' @inheritParams cdf 
#' 
#' @return The standard deviation and variance of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#'     Both quantities can be evaluated at a vector. 
#' 
#' @details \code{sd_stressed}: The standard deviation of 
#'      a chosen stressed model component, subject to the calculated scenario weights.
#'      The standard deviation of a stressed model component
#'      is denoted as \deqn{sd^W}.
#'
#'      The function \code{sd_stressed} provides stressed standard deviation, whereas
#'      the function \code{var_stressed} calculates stressed variance of model
#'      components with different interpolations.
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed standard deviation
#' sd_stressed(res1, xCol = 1, wCol = 1)
#' ## baseline standard deviation
#' sd(x$normal)
#' 
#' @author Kent Wu
#' @describeIn sd_stressed Standard deviation and var_stressed
#'     variance of stressed model components
#' 
#' @seealso See \code{\link{stress_moment}} stressing a baseline 
#'     model with desired moment constraints.
#' @export

sd_stressed <- function(object, xCol = 1, wCol = 1){
  if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  new_weights <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  sd <- .sd(x = x_data, w = new_weights)
  return(sd)
}

var_stressed <- function(object, xCol = 1, wCol = 1){
  var <- sd_stressed(object, xCol = 1, wCol = 1)^2
  return(sd)
}

# tst 

# help function 
# x    numeric vector  
# w    numeric vector with weights
.sd <- function(x, w){
  n <- length(as.vector(x))
  mean_w <- stats::weighted.mean(x = x, w = w)
  sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
  return(sd_w)
}