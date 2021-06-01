#' Standard Deviation and Variance of a Stressed Model
#' 
#' Provides the standard deviation and variance of stressed 
#'     model components (random variables) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM  
#' 
#' @return \code{sd_stressed}: Return the standard deviation of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#'     The quantity can be evaluated at a vector. 
#' 
#' @details \code{sd_stressed}: The standard deviation of 
#'      a chosen model component, subject to the calculated scenario weights.
#'
#'      The function \code{sd_stressed} provides stressed standard deviationw
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed standard deviation
#' sd_stressed(res1, xCol = 1, wCol = 1)
#' ## baseline standard deviation
#' sd(x$normal)
#' 
#' ## stressed variance
#' var_stressed(res1, xCol = 1, wCol = 1)
#' ## baseline variance
#' var(x$normal)
#' 
#' @author Kent Wu
#' @describeIn sd_stressed Standard deviation of model components
#' 
#' @seealso See \code{\link{mean_stressed}} for means of stressed model components,
#' and \code{\link{cor_stressed}} for correlations between stressed model components. 
#' 
#' @export

sd_stressed <- function(object, xCol = 1, wCol = 1, base=FALSE){
  mean_w <- mean_stressed(object, xCol, wCol, base)
  cname <- colnames(mean_w)
  rname <- rownames(mean_w)

  x_data <- as.matrix(get_data(object, xCol = xCol))
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- as.matrix(get_weights(object)[ ,wCol])
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    new_weights <- cbind(old_weights, new_weights)
  }
  
  n <- dim(x_data)[1] # number of observations
  m <- dim(mean_w)[1] # number of weights
  d <- dim(mean_w)[2] # number of random variables
  
  temp <- do.call(rbind, lapply(1:m, function(i) (x_data - rep(mean_w[i,], each=n))^2))

  # print(dim(x_data))
  # print(dim(mean_w))
  # print(dim(new_weights))
  # print(dim(temp))

  dim(new_weights) <- c(n, m, 1)
  dim(temp) <- c(n, m, d)

  sd <- do.call(rbind, lapply(1:m, function(i) sqrt(t(new_weights[,i,]) %*% temp[,i,] /(n-1) )))
  colnames(sd) <- cname
  rownames(sd) <- rname
  
  return(sd)
}

#' @describeIn sd_stressed variance of model components
#' 
#' @return \code{var_stressed}: Return the variance of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#'     The quantity can be evaluated at a vector. 
#'     
#' @details \code{var_stressed}: The variance of 
#'      a chosen stressed model component, subject to the calculated scenario weights.
#'
#'      The variance of a stressed model component is denoted as \deqn{var^W}
#'
#' @export

var_stressed <- function(object, xCol = 1, wCol = 1, base=FALSE){
  var <- (sd_stressed(object, xCol = 1, wCol = 1, base))^2
  return(var)
}
