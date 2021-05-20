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
