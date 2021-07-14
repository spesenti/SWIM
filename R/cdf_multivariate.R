#' Empirical Distribution Function of a Stressed Model
#' 
#' Provides the empirical distribution of a stressed model component (random variable) under the scenario weights
#' 
#' @inheritParams sensitivity 
#' @param xCol    Vector or characters, (names of) the columns of the underlying data 
#'                of the \code{object} (\code{default = "all"}). 
#' @param wCol    Numeric, the columns of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' @param grid    A matrix containing the empirical distribution of the xCol components 
#'                of the stressed model with weights wCol. The empirical distribution 
#'                function can be evaluated at a vector valued xCol.
#' @param base    Logical, if TRUE, statistics under the baseline are also returned (default = "FALSE").]
#' 
#' @details The \code{cdf_stressed} returns the values of the empirical distribution function 
#'          of the xCol model components for weights wCol. In contrast, the \code{cdf} function 
#'          returns a function, analogous to the ecdf from the base package. The 
#'          function \code{cdf_stressed} is the \code{cdf} function applied to grid.
#' 
#' @return A matrix containing the empirical distribution function 
#'         applied to \code{grid} of the xCol components of the 
#'         stressed model with weights wCol
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' grid <- cbind(seq(min(x$normal), max(x$normal), length.out = 5), 
#'               seq(min(x$gamma), max(x$gamma), length.out = 5))
#' ## stressed empirical distribution function
#' cdf_stressed(res1, xCol = "all", wCol = 1, grid = grid)
#' 
#' @author Kent Wu
#' 
#' @seealso See \code{\link{cdf}} for the empirical distribution function of a stressed 
#'     model component and \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model. 
#' @export
#' 

cdf_stressed <- function(object, xCol = "all", wCol = 1, grid, base=FALSE){
  if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")

  if (length(wCol) > 1 || wCol == "all") stop("Input wCol has dimension larger than 1")
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  new_weights <- as.matrix(get_weights(object)[ , wCol]) # single weight
  
  if (ncol(as.matrix(x_data)) != ncol(as.matrix(grid))) stop("The number of model components does match with input dimension of grid")
  cdf <- .cdf_stressed(x = x_data, w = new_weights, grid = grid)
  
  if (is.null(colnames(get_data(object)))){
    cname <-  paste("X", as.character(xCol), sep = "")
  } else if (!is.character(xCol)){
    cname <- colnames(get_data(object))[xCol]
  } else {
    cname <- xCol   
  }
  colnames(cdf) <- cname
  
  if (base == TRUE){
    old_weights <- as.matrix(rep(1, length(x_data[,1])), ncol = 1)
    cdf_base <- .cdf_stressed(x = x_data, w = old_weights, grid = grid)
    colnames(cdf_base) <- paste("base", cname, sep=' ')
    cdf <- cbind(cdf, cdf_base)
  }
  return(cdf)
}


# help function 
# x    numeric vector  
# w    numeric vector with weights
.cdf_stressed <- function(x, w, grid){
  d <- ncol(x)
  t <- as.matrix(grid)
  
  res <- matrix(NA, ncol=d, nrow=nrow(t))
  for (i in 1:d) {res[, i] <- t(w) %*% sapply(t[, i], FUN = function(s) x[, i] <= s) / nrow(x)}
  return(res)
}
