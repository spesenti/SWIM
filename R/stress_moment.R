#' Stressing Moments
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that stressed model components (random variables) 
#'     fulfil the moment constraints. Scenario weights are selected by 
#'     constrained minimisation of the relative entropy to the 
#'     baseline model.
#'     
#' @inheritParams   stress_VaR
#' @param f         A function, or list of functions, that, applied to 
#'                  \code{x}, constitute the moment constraints.
#' @param k         A vector or list of vectors, same length as \code{f},
#'                  indicating which columns of
#'                  \code{x} each function in \code{f} operates on.
#' @param m         Numeric vector, same length as \code{f}, containing
#'                  the stressed moments of \code{f(x)}. Must be in the
#'                  range of \code{f(x)}.
#' @param ...       Additional arguments to be passed to 
#'                  \code{\link[nleqslv]{nleqslv}}.
#' 
#' @details When \code{f} is a list, \code{k[[i]]} corresponds to the input 
#'     variables of \code{f[[i]]}. 
#' 
#'     The moment constraints are given by \code{E^Q( f(x) ) = m}, 
#'     where \code{E^Q} denotes the expectation under the stressed 
#'     model. \code{stress_moment} solves the subsequent set of equations 
#'     with respect to theta, using \code{\link[nleqslv]{nleqslv}}:
#'     
#'     \deqn{E^Q( f(x) ) = E( f(x) * exp(theta * f(x)) ) = m.}
#'     
#'     There is no guarantee that the set of equations 
#'     will have a solution, or that the solution is unique. 
#'     
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list, each component corresponds to 
#'    a different stress and is a vector of scenario weights;
#'      \item \code{type = "moment"};
#'       \item \code{specs}, a list, each component corresponds to 
#'    a different stress and contains \code{f}, \code{k} and \code{m}.
#'     }
#'     See \code{\link{SWIM}} for details.
#'     
#' @examples 
#' set.seed(0)
#' x <- data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2), 
#'   "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))
#'   
#' ## stressing covariance of columns 1,2 while leaving the means unchanged
#' res1 <- stress_moment(x = x, 
#'   f = list(function(x)x, function(x)x, function(x)x[1] * x[2]), 
#'   k = list(1, 2, c(1, 2)), m = c(0, 2, 0.5), 
#'   method = "Newton", control = list(maxit = 1000, ftol = 1E-10))
#' ## means under the stressed model
#' summary(res1)
#' apply(x, 2, stats::weighted.mean, w = get_weights(res1)) 
#' ## covaraince of columns 1,2 under the stressed model
#' stats::weighted.mean(x[, 1] * x[, 2], w = get_weights(res1))
#'     
#' ## stressing jointly the tail probabilities of columns 1,3  
#' res2 <- stress_moment(x = x, 
#'   f = list(function(x)(x > 1.5), function(x)(x > 0.9)), 
#'   k = c(1, 3), m = c(0.9, 0.9))
#' summary(res2)
#' ## probabilities under the stressed model
#' mean((x[, 1] > 1.5) * get_weights(res2))
#' mean((x[, 3] > 0.9) * get_weights(res2))
#'      
#' @family stress functions 
#' 
#' @seealso See \code{\link{stress_mean}} for stressing means and 
#'     \code{\link{stress_mean_sd}} for stressing mean and standard 
#'     deviation jointly. 
#' @inherit SWIM references 
#' @export

stress_moment <- function(x, f, k, m, ...){
  if (is.SWIM(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  # check if x is not a vector, matrix or data.frame.
  if (anyNA(x_data)) warning("x contains NA")
  if (is.function(f)) f <- as.list(f)
  if (!all(sapply(f, is.function))) stop("f must be a list of functions")
  if (is.numeric(k)) k <- as.list(k)
  if (!all(sapply(k, is.numeric))) stop("k must be a list of numeric vectors")
  if (!is.numeric(m)) stop("m must be numeric")
  if ((length(m) != length(f)) || (length(m) != length(k)) || (length(f) != length(k))) stop("Objects f, k and m must have the same length.")
  z <- matrix(0, ncol = length(f), nrow = nrow(x_data))
  for (i in 1:length(f)){
    z[, i] <- apply(x_data[, k[[i]], drop = FALSE], 1, f[[i]])
  }
  min.fz <- apply(z, 2, min)
  max.fz <- apply(z, 2, max)
  if (any(m < min.fz) || any(m > max.fz)) stop("Values in m must be in the range of f(x).")
  z <- cbind(1, z)
  moments <- function(x)colMeans(z * as.vector(exp(z %*% x))) - c(1, m)
  sol <- nleqslv::nleqslv(rep(0, length.out = length(f) + 1), moments, ...)
  if (sol$termcd != 1) stop(paste("nleqslv could not find a solution and terminated with code ", sol$termcd))
  constr_moment <- list("k" = k, "m" = m, "f" = f)
  constr <- list(constr_moment)
  names(constr) <- paste("stress", 1)
  new_weights <- list("stress 1" = as.vector(exp(z %*% sol$x)))
  type <- list("moment")
  my_list <- SWIM("x" = x, "new_weights" = new_weights, "type" = type, "specs" = constr)
  return(my_list)
  }

#' Stressing Means
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that stressed model components (random variables) 
#'     fulfil the mean constraints. Scenario weights are selected by  
#'     constrained minimisation of the relative entropy to the 
#'     baseline model.
#'    
#' @inheritParams   stress_moment
#' @param k         Numeric vector, the columns of \code{x} that
#'                  are stressed.
#' @param new_means Numeric vector, same length as \code{k}, 
#'                  containing the stressed means. 
#'                  
#' @details The function \code{stress_mean} is a wrapper for the 
#'     function \code{stress_moment}. See \code{\link{stress_moment}} 
#'     for details on the additional arguments to \code{...} and 
#'     the underlying algorithm.
#' 
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list, each component corresponds to 
#'    a different stress and is a vector of scenario weights;
#'      \item \code{type = "mean"};
#'       \item \code{specs}, a list, each component corresponds to 
#'    a different stress and contains \code{k} and \code{new_means}.
#'    }
#'    See \code{\link{SWIM}} for details.
#'     
#' @examples 
#' set.seed(0)
#' x <- data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2), 
#'   "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))
#' ## stressing means
#' res1 <- stress(type = "mean", x = x, k = 1 : 3, 
#'   new_means = c(1, 1, 0.75))
#' summary(res1)
#' res1$specs
#' ## calling stress_mean directly
#' res2 <- stress_mean(x = x, k = 1 : 3, 
#'   new_means = c(1, 1, 0.75))
#' summary(res2)
#' 
#' ## See also examples in \code{\link{stress_moment}} and \code{\link{stress_mean_sd}}.     
#'     
#' @family stress functions 
#' @seealso See \code{\link{stress_mean_sd}} for stressing means 
#'     and standard deviations jointly, and \code{\link{stress_moment}} for 
#'     moment constraints.   
#' 
#' @inherit SWIM references 
#' @export

stress_mean <- function(x, k, new_means, ...)
{
  means <- rep(list(function(x)x), length(k))
  res <- stress_moment(x = x, f = means, k = k, m = new_means, ...)
  res$type <- list("mean")
  res$specs$`stress 1` <- list("k" = k, "new_means" = new_means)
  return(res)
}


#' Stressing Mean and Standard Deviation
#'
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that stressed model components (random variables) 
#'     fulfil the mean and standard deviation constraints. 
#'     Scenario weights are selected by constrained minimisation of 
#'     the relative entropy to the baseline model.
#'    
#' @inheritParams   stress_mean
#' @param new_sd    Numeric vector, same length as \code{k}, 
#'                  containing the stressed standard deviations. 
#' 
#' @details The function \code{stress_mean_sd} is a wrapper for the 
#'     function \code{stress_moment}. See \code{\link{stress_moment}} 
#'     for details on the additional arguments to \code{...} and 
#'     the underlying algorithm.
#'     
#'     For stressing means only, see \code{\link{stress_mean}}, 
#'     for stressing highter moments and functions of moments, 
#'     see \code{\link{stress_moment}}.
#'     
#' 
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list, each component corresponds to 
#'    a different stress and is a vector of scenario weights;
#'      \item \code{type = "mean"};
#'       \item \code{specs}, a list, each component corresponds to 
#'    a different stress and contains \code{k}, \code{new_means} and
#'    \code{new_sd}.
#'    }
#'    See \code{\link{SWIM}} for details.
#'  
#' @examples 
#' set.seed(0)
#' x <- data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2), 
#'   "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))
#' ## stressing mean and sd of column 1
#' res1 <- stress(type = "mean sd", x = x, k = 1, new_means = 0.1, 
#'   new_sd = 1.1, method = "Newton", 
#'   control = list(maxit = 1000, ftol = 1E-15))
#' summary(res1)
#' ## calling stress_mean_sd directly 
#' res2 <- stress_mean_sd(x = x, k = 1, new_means = 0.1, 
#'   new_sd = 1.1, method = "Newton", 
#'   control = list(maxit = 1000, ftol = 1E-15))

#' 
#' ## See also examples in \code{\link{stress_moment}}.     
#'           
#' @family stress functions 
#' @inherit SWIM references 
#' @export 


# k, new_means, new_sd have to be the same length
# one can only stress the mean and sd together. 
stress_mean_sd <- function(x, k, new_means, new_sd, ...)
{
  means <- rep(list(function(x)x), length(k))
  second_moments <- rep(list(function(x)x ^ 2), length(k))
  f <- c(means, second_moments)
  m <- c(new_means, new_means ^ 2 + new_sd ^ 2)
  k_new <- c(k, k)
  res <- stress_moment(x, f, k_new, m, ...)
  res$type <- list("mean sd")
  res$specs$`stress 1` <- list("k" = k, "new_means" = new_means, "new_sd" = new_sd)
  return(res)
}
