#' Empirical Distribution Function of a Stressed Model
#' 
#' Provides the empirical distribution function of a stressed 
#'     model component (random variable) under the scenario weights. 
#' 
#' @inheritParams sensitivity 
#' @param xCol    Vector or characters, (names of) the columns of the underlying data 
#'                of the \code{object} (\code{default = 1}). 
#' @param wCol    Vector, the columns of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' 
#' @return The empirical distribution function (a function) of 
#'     the \code{xCol} component of the stressed model with weights 
#'     \code{wCol}. The empirical distribution function can be 
#'     evaluated at a vector. 
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' grid <- seq(min(x$normal), max(x$normal), length.out = 5)
#' ## stressed empirical distribution function
#' cdf(res1, xCol = 1, wCol = 1)(grid)
#' ## baseline empirical distribution function
#' ecdf(x$normal)(grid)
#' 
#' @author Silvana M. Pesenti 
#' 
#' @seealso See \code{\link{plot_cdf}} for plotting the empirical 
#'     distribution function of the stressed model and 
#'     \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model. 
#' @export

  cdf <- function(object, xCol = 1, wCol = 1){
   if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
   if (anyNA(object$x)) warning("x contains NA")
   new_weights <- get_weights(object)[ , wCol]
   x_data <- get_data(object)[ , xCol]
   cdf <- .cdf(x = x_data, w = new_weights)
   return(cdf)
  }
  # tst 

 # help function 
 # x    numeric vector  
 # w    numeric vector with weights
  .cdf <- function(x, w){
    .cdf_stress <- function(t) colMeans(w * (sapply(t, FUN = function(s) x <= s)))
   return(.cdf_stress)
  }
