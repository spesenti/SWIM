sd <- function(object, xCol = 1, wCol = 1){
  if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  new_weights <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  sd <- .sd(x = x_data, w = new_weights)
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