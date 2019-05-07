# X matrix, vector or data.frame
# k list of vectors indicating which columns of X each function in f operates
# m vector of values of same length as f
# must be in the range of f(x)

# the function calculates the solution of the set of equations
# E[f1(X)]=m1, ... ,E[fk(X)]=mk

# ... additional arguments to be passed to nleqslv

stress_moments <- function(x, f, k, m, ...)
{
  if(is.data.frame(x) | is.vector(x)) x <- as.matrix(x)

  if(is.function(f)) f <- as.list(f)
  if(!all(sapply(f, is.function))) stop("'f' must be a list of functions")
  
  if(is.numeric(k)) k <- as.list(k)
  if(!all(sapply(k, is.numeric))) stop("'k' must be a list of numeric vectors")
  
  if(!is.numeric(m)) stop("'m' must be numeric")
  if((length(m) != length(f)) || (length(m) != length(k)) || (length(f) != length(k))) stop("objects 'f', 'k' and 'm' must have the same length")

  z <- matrix(0, ncol = length(f), nrow = nrow(x))

  for(i in 1 : length(f))
  {
    z[, i] <- apply(x[, k[[i]], drop = FALSE], 1, f[[i]])
  }
  
  min.fz <- apply(z, 2, min)
  max.fz <- apply(z, 2, max)
  if (any(m < min.fz) || any(m > max.fz)) stop("values in 'm' must be in the range of f(x)")

  z <- cbind(1, z)
  
  csiszarFUN <- function(x)colMeans(z * as.vector(exp(z %*% x))) - c(1, m)
  
  require(nleqslv, quietly = TRUE)
  sol <- nleqslv(x = rep(0, length.out = length(f) + 1), fn = csiszarFUN, ...)
  
  if (sol$termcd != 1) stop(paste("nleqslv could not find a solution and terminated with code ", sol$termcd))
  
  new_weights <- function(z)as.vector(exp(c(1, z) %*% sol$x))
  
  # new_weights <- as.vector(exp(z %*% sol$x))
  # new_weights <- new_weights / sum(new_weights)

  constr <- list(f, m)
  
  specs <- list(type = "moments", "k" = k, "constr" = constr)
  my_list <- SWIM("x" = x, "new_weights" = new_weights, "specs" = specs)
  return(my_list)
  }


set.seed(0)
Y <- cbind(rnorm(1000), rgamma(1000, shape = 2), rbeta(1000, shape1 = 2, shape2 = 2))
colnames(Y) <- c("normal", "gamma", "beta")

VaR(Y)

# debug(stress_moments)

res1 <- stress_moments(x = Y, f = rep(list(function(x)x), 3), k = 1 : 3, m = c(1, 1, 0.75))
apply(X = Y, MARGIN = 2, weighted.mean, w = apply(Y, 1, res1$new_weights))


res2 <- stress_moments(x = Y, f = rep(list(function(x)x[1] * x[2]), 3), k = list(c(1, 2), c(1, 3), c(2, 3)), m = c(0.5, 0.3, 0.2))
weighted.mean(Y[, 1] * Y[, 2], w = res2$new_weights)
weighted.mean(Y[, 1] * Y[, 3], w = res2$new_weights)
weighted.mean(Y[, 2] * Y[, 3], w = res2$new_weights)


res3 <- stress_moments(x = Y, f = list(function(x)prod(x)), k = list(c(1, 2, 3)), m = 1)
weighted.mean(Y[, 1] * Y[, 2] * Y[, 3], w = res3$new_weights)


res4 <- stress_moments(x = Y, f = list(function(x)exp(x[1] - x[2]), function(x)x[1] * exp(- x[2])), k = list(c(1, 2), c(2, 3)), m = c(1, 0.5))
weighted.mean(exp(Y[, 1] - Y[, 2]), w = res4$new_weights)
weighted.mean(Y[, 2] * exp(- Y[, 3]), w = res4$new_weights)

res5 <- stress_moments(x = Y, f = list(function(x)x, function (x)x, function(x)x ^ 2, function (x)x ^ 2), k = list(), m = c())
weighted.mean(exp(Y[, 1] - Y[, 2]), w = res4$new_weights)
weighted.mean(Y[, 2] * exp(- Y[, 3]), w = res4$new_weights)

# X matrix, vector or data.frame
# k vector indicating which columns of x should be stressed
# m vector of stressed means of same length as k
# must be in the range of x

# ... additional arguments to be passed to nleqslv
stress_means <- function(x, k, new_means, ...)
{
  
  means <- rep(list(function(x)x), length(k))
  
  res <- stress_moments(x, means, k, new_means, ...)

  return(res)
}

res1 <- stress_means(Y, k = c(1, 2), m = c(1, 1))

apply(Y, 2, weighted.mean, w = apply(Y[,1:2], 1, res1$new_weights))

stress_means_sd <- function(x, k, new_means, new_sd, ...)
{

  means <- rep(list(function(x)x), length(k))
  second_moments <- rep(list(function(x)x ^ 2), length(k))
  
  f <- c(means, second_moments)
  m <- c(new_means, new_means ^ 2 + new_sd ^ 2)
  
  res <- stress_moments(x, f, k, m, ...)
  
  return(res)
}

