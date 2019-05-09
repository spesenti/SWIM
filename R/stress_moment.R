# x matrix, vector or data.frame
# function, or list of functions
# k vector or list of vectors indicating which columns of X each function in f operates
# m vector of values of same length as f containing the new moments of f(x); must be in the range of f(x)
# the function calculates the solution wrt theta of the set of equations
# EQ[f(x)]=E[f(x)exp(theta * f(x))]=m

# ... additional arguments to be passed to nleqslv

stress_moment <- function(x, f, k, m, ...){
  if(is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  # check if x is not a vector, matrix or data frame?
  if(anyNA(x_data)) warning("'x' contains NA")
  if(is.function(f)) f <- as.list(f)
  if(!all(sapply(f, is.function))) stop("'f' must be a list of functions")
  if(is.numeric(k)) k <- as.list(k)
  if(!all(sapply(k, is.numeric))) stop("'k' must be a list of numeric vectors")
  if(!is.numeric(m)) stop("'m' must be numeric")
  if((length(m) != length(f)) || (length(m) != length(k)) || (length(f) != length(k))) stop("objects 'f', 'k' and 'm' must have the same length")
  z <- matrix(0, ncol = length(f), nrow = nrow(x_data))
  for(i in 1 : length(f))
  {
    z[, i] <- apply(x_data[, k[[i]], drop = FALSE], 1, f[[i]])
  }
  min.fz <- apply(z, 2, min)
  max.fz <- apply(z, 2, max)
  if (any(m < min.fz) || any(m > max.fz)) stop("values in 'm' must be in the range of f(x)")
  z <- cbind(1, z)
  moments <- function(x)colMeans(z * as.vector(exp(z %*% x))) - c(1, m)
  require(nleqslv, quietly = TRUE)
  sol <- nleqslv(rep(0, length.out = length(f) + 1), moments, ...)
  if (sol$termcd != 1) stop(paste("nleqslv could not find a solution and terminated with code ", sol$termcd))
  new_weights <- function(w)as.vector(exp(c(1, w) %*% sol$x))
  constr <- list(f, m)
  specs <- list(type = "moments", "k" = k, "constr" = constr)
  my_list <- SWIM("x" = x, "new_weights" = new_weights, "specs" = specs)
  return(my_list)
  }


# x matrix, vector or data.frame
# k vector indicating which columns of x should be stressed
# m vector of stressed means of same length as k
# must be in the range of x
# ... additional arguments to be passed to nleqslv
stress_mean <- function(x, k, new_means, ...)
{
  means <- rep(list(function(x)x), length(k))
  res <- stress_moment(x, means, k, new_means, ...)
  return(res)
}


# k, new_means, new_sd have to be the same length
# one can only stress the mean and sd together. 
stress_mean_sd <- function(x, k, new_means, new_sd, ...)
{
  means <- rep(list(function(x)x), length(k))
  second_moments <- rep(list(function(x)x ^ 2), length(k))
  f <- c(means, second_moments)
  m <- c(new_means, new_means ^ 2 + new_sd ^ 2)
  k <- c(k, k)
  res <- stress_moment(x, f, k, m, ...)
  return(res)
}
