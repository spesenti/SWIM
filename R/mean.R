#' Mean of a Stressed Model
#' 
#' Provides the mean of stressed model components (random variables) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM 
#' 
#' @return A matrix containing the means of the \code{xCol}
#'     components of the stressed model with weights \code{wCol}.
#' 
#' @details \code{mean_stressed}: Sample mean of chosen stressed model components, subject to the calculated scenario weights.
#'
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed mean
#' mean_stressed(res1, xCol = "all", wCol = "all", base = TRUE)
#' 
#' @author Kent Wu
#' 
#' @seealso See \code{\link{var_stressed}} and \code{\link{sd_stressed}} compute
#'     stressed variance and standard deviations under the scenario weights, respectively.
#'     
#' @export

mean_stressed <- function(object, xCol = "all", wCol = "all", base=FALSE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class 'SWIM' or 'SWIMw'")
  if (anyNA(object$x)) warning("x contains NA")
  
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- as.matrix(get_weights(object)[ ,wCol])  
  
  n <- dim(x_data)[1]
  mean <- t(new_weights) %*% x_data / n
  colnames(mean) <- cname
  rownames(mean) <- names(object$specs)[wCol]
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    mean_base <- t(old_weights) %*% x_data / n
    mean <- rbind(mean_base, mean)
    rownames(mean) <- c("base", names(object$specs)[wCol])
  }

  return(mean)
}

