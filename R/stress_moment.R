#' Stressing moments
#'
#' Provides scenario weights such that the random variable
#'    under the new scenraio weights fulfils the moment constraints and
#'    has minimal Kullback-Leibler divergence to the baseline random
#'    variable.

#' @inheritParams   stress_VaR
#' @param  k        A vector or list of vectors indicating which columns of
#'                  \code{x} each function in \code{f} operates on.
#' @param f         A function, or list of functions.
#' @param m         Vector of values, same length as \code{f}, containing
#'                  the new moments of \code{f(x)}. Must be in the range of
#'                  \code{f(x)}.
#' @param ...       Additional arguments to be passed to 
#'                  \code{\link[nleqslv]{nleqslv}}.
#' 
#' @details Calcualtes the solution wrt theta of the set of equations
# EQ[f(x)]=E[f(x)exp(theta * f(x))]=m.
#' 
#' @return A \code{\link{SWIM}} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a list of functions, that applied to
#'       the \code{k}th colums of \code{x} generate the vectors of the
#'       new weights;
#'       \item \code{specs}, the specification of what has been
#'       stressed.
#'       The \code{specs} is a data.frame consisting of ???
#'     }
#'     
#' @family stress functions 
#' @export

stress_moment <- function(x, f, k, m, ...){
  if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  # check if x is not a vector, matrix or data frame?
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
  require(nleqslv, quietly = TRUE)
  sol <- nleqslv(rep(0, length.out = length(f) + 1), moments, ...)
  if (sol$termcd != 1) stop(paste("nleqslv could not find a solution and terminated with code ", sol$termcd))
  new_weights <- function(w)as.vector(exp(c(1, w) %*% sol$x))
  constr <- list(f, m)
  specs <- list(type = "moments", "k" = k, "constr" = constr)
  my_list <- SWIM("x" = x, "new_weights" = new_weights, "specs" = specs)
  return(my_list)
  }

#' Stressing means
#'
#' Provides scenario weights such that the random variable
#'    under the new scenraio weights fulfils the mean constraints and
#'    has minimal Kullback-Leibler divergence to the baseline random
#'    variable.
#'    
#' @inheritParams   stress_moment
#' @param new_means Numeric vector, same length as \code{k}, 
#'                  containing the stressed means. 
#' @param k         Numeric vector, the
#'                  column of \code{x} that are stressed.
#' @details 
#' 
#' @return A \code{\link{SWIM}} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a list of functions, that applied to
#'       the \code{k}th colum of \code{x} generate the vectors of the
#'       new weights;
#'       \item \code{specs}, the specification of what has been
#'       stressed.
#'       The \code{specs} is a data.frame consisting of ???
#'     }
#'     
#' @family stress functions 
#' @export

stress_mean <- function(x, k, new_means, ...)
{
  means <- rep(list(function(x)x), length(k))
  res <- stress_moment(x, means, k, new_means, ...)
  return(res)
}

#' Stressing mean and standard deviation
#'
#' Provides scenario weights such that the random variable
#'    under the new scenraio weights fulfils the mean and standard
#'    deviation constraints and has minimal Kullback-Leibler divergence 
#'    to the baseline random variable.
#'    
#' @inheritParams   stress_mean
#' @param new_sd    Numeric vector, same length as \code{k}, 
#'                  containing the stressed standard deviations. 
#' 
#' @details 
#' 
#' @return A \code{\link{SWIM}} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a list of functions, that applied to
#'       the \code{k}th colum of \code{x} generate the vectors of the
#'       new weights;
#'       \item \code{specs}, the specification of what has been
#'       stressed.
#'       The \code{specs} is a data.frame consisting of ???
#'     }
#'     
#' @family stress functions 
#' @export 


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
