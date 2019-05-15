#' Stressed Empirical Distribution Function
#' @export
# function calculating the distorted empirical distribution function
# Note that the ecdf is not a generic, so we cannot define a method.

# object      SWIM object
# xCol   integer numeric, which colum the ecdf is to be calcuated, (default = 1).
# wCol   integer numeric, column of new_weights, (default = 1)

# the ecdf returns the empirical distribution function that can be evaluated on a vector. 

  cdf <- function(object, xCol = 1, wCol = 1){
   if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
   if (anyNA(object$x)) warning("x contains NA")
   new_weights <- get.weights(object)[ , wCol]
   x_data <- get.data(object)[ , xCol]
   cdf <- .cdf(x = x_data, w = new_weights)
   return(cdf)
  }

 # help function 
 # x    numeric vector  
 # w    numeric vector with weights
  .cdf <- function(x, w){
    .cdf_stress <- function(t) colMeans(w * (sapply(t, FUN = function(s) x <= s)))
   return(.cdf_stress)
  }
