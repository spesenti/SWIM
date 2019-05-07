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

# get.weight.moment <- function(x)

set.seed(0)
x <- cbind(rnorm(1000), rgamma(1000, shape = 2), rbeta(1000, shape1 = 2, shape2 = 2))
colnames(x) <- c("normal", "gamma", "beta")

# example with stress_var
apply(x, 2, quantile, p = 0.9)
apply(x, 2, range)
res1 <- stress_VaR(x = x, alpha = 0.9, q_ratio = c(1.1, 1.2), k = 1)
# undebug(stress_VaR)
all.equal(get.data(res1), x)
get.specs(res1)
get.weights(res1)
get.weightsfun(res1)[[1]]

# example with stress_mean
colMeans(x)
apply(x, 2, range)
res2 <- stress_means(x = res1, k = 1 : 3, new_means = c(1, 1, 0.75))
apply(X = x, MARGIN = 2, weighted.mean, w = apply(x, 1, res2$new_weights))
# debug(stress_moments)

# example with stress_mean_sd; stress mean and sd of col 1
colMeans(x)
apply(x, 2, sd)
res3 <- stress_mean_sd(x = x, k = 1, new_means = 0.1, new_sd = 1.1, method = "Newton", control = list(maxit = 1000, ftol = 1E-15))
www <- apply(cbind(x[, 1], x[, 1] ^ 2), 1, res3$new_weights)
mean(www)
weighted.mean(x = x[, 1], w = www)
sqrt(weighted.mean(x = x[, 1] ^ 2, w = www) - weighted.mean(x = x[, 1], w = www) ^ 2)
debug(stress_mean_sd)
debug(stress_moment)

# example with stress_mean_sd; stress mean and sd of all cols
colMeans(x)
apply(x, 2, sd)
res4 <- stress_mean_sd(x = x, k = 1 : 3, new_means = c(0.1, 3, 0.7), new_sd = c(1.1, 1.6, 0.20), method = "Newton", control = list(maxit = 1000, ftol = 1E-10))
www <- apply(cbind(x, x ^ 2), 1, res4$new_weights)
mean(www)
apply(x, 2, weighted.mean, w = www)
sqrt(apply(x ^ 2, 2, weighted.mean, w = www) - apply(x, 2, weighted.mean, w = www) ^ 2)
undebug(stress_mean_sd)
undebug(stress_moment)

# stressing covariance of X1, X2 while leaving the means unchanged
colMeans(x)
res5 <- stress_moment(x = x, f = list(function(x)x,  function(x)x, function(x)x[1] * x[2]), k = list(1, 2, c(1, 2)),  m = c(0, 2, 0.5), method = "Newton", control = list(maxit = 1000, ftol = 1E-10))
www <- apply(cbind(x[, 1 : 2], x[, 1] * x[, 2]), 1, res5$new_weights)
mean(www)
apply(x, 2, weighted.mean, w = www)
weighted.mean(x[, 1] * x[, 2], w = www)

# stressing jointly the VaR of X1 and X3
apply(x, 2, quantile, p = 0.9)
# set new 90% VaR for X1 at 1.5, for X3 at 0.9
res6 <- stress_moment(x = x, f = list(function(x)(x > 1.5), function(x)(x > 0.9)), k = c(1, 3), m = c(0.9, 0.9))
www <- apply(cbind((x[, 1] > 1.5), (x[, 3] > 0.9)), 1, res6$new_weights)
mean(www)
mean((x[, 1] > 1.5) * www)
mean((x[, 3] > 0.9) * www)


# example with stress_user
res7 <- stress_user(x = x, new_weights = 

# example with stress prob
quantile(x[, 1], probs = c(0.25, 0.5, 0.75))
# set new quartiles for X1 at -1, -0.3, 1
res7 <- stress_prob(x = x, prob = c(0.25, 0.25, 0.25), upper = c(-1, -0.3, 1), k = 1)
debug(stress_prob)
