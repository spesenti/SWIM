#' Correlation of a Stressed Model
#' 
#' Provides the correlation of stressed model components
#'     (random variable) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM
#' @param method  Character, one of \code{"pearson", "spearman", "kendall"}. (\code{default = "pearson"}).
#' 
#' @return \code{cor_stressed} returns a list of correlation matrices
#'     corresponding to different stresses. Entries of the matrices denote 
#'     correlation coefficients between stressed model components specified by \code{xCol}. 
#' 
#' @details \code{cor_stressed}: The correlation coefficient of  
#'      stressed model components, subject to the calculated scenario weights.
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
#' ## stressed correlation
#' cor_stressed(res1, xCol = c(1, 2), wCol = 1, base=TRUE)
#' 
#' @author Kent Wu
#' @describeIn cor_stressed correlation coefficient of stressed model components
#' 
#' @seealso See \code{\link{var_stressed}} and \code{\link{sd_stressed}} compute
#'     stressed variance and standard deviations under the scenario weights, respectively.
#'          
#'     See \code{\link{cor_stressed}} for correlations between stressed model components.
#'     
#' @export

cor_stressed <- function(object, xCol = c(1, 2), wCol = "all", method = "pearson", base=FALSE){
  if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  if (!(method %in% c("pearson", "spearman", "kendall"))) stop("Method must be one of pearson, spearman and kendall")
  
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)

  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- get_weights(object)[ ,wCol]  
  
  corr_w <- apply(X = as.matrix(new_weights), MARGIN = 2, 
                  FUN = .cor_helper, x_data = x_data, method = method)
  names(corr_w) <- paste("stress", wCol)
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    corr_base <- .cor_helper(x_data = x_data, w = old_weights, method = method)
    corr_w <- c(list("base" = corr_base), corr_w)
  }
  
  for (i in 1:length(corr_w)){
    colnames(corr_w[[i]]) <- cname
    rownames(corr_w[[i]]) <- cname
  }
  return (corr_w)
}

.cor_helper <- function(x_data, w, method){
  d <- ncol(x_data)
  mat <- matrix(NA, nrow=d, ncol=d)
  # corr_w <- outer(1:d, 1:d, FUN = function(i,j) .corr(x_data[,c(i, j)], w, method))
  # corr_w <- mapply(function(i,j) .corr(x_data[,c(i, j)], w, method), row(mat), col(mat))
  for (i in 1:d){
    for (j in 1:d){
      mat[i,j] <- .corr(x_data[,c(i, j)], w, method)
    }
  }
  return (data.frame(mat))
} 

.corr <- function(x, w, method){
  x1 <- x[, 1]; x2 <- x[, 2]
  if (method == "pearson") {
    res <- .pearson(x1,x2,w)
  }
  if (method == "kendall") {
    res <- stats::cor(x1*w, x2*w, method = method)
    # res <- .tau(x1*w, x2*w)
  }
  if (method == "spearman") {
    x_rank <- rank(x1)
    y_rank <- rank(x2)
    res <- .pearson(x_rank, y_rank, w)
  }
  return (res)
}

.moments <- function(x, w){
  n <- length(as.vector(x))
  mean_w <- stats::weighted.mean(x = x, w = w)
  sd_w <- sqrt(mean(w * (x - mean_w)^2) * n / (n-1)) 
  moments_w <- rbind(mean_w, sd_w)
  return(moments_w)
}

.pearson <- function(x1, x2, w){
  n <- length(w)
  moments_x1 <- .moments(x1, w)
  moments_x2 <- .moments(x2, w)
  m1 <- moments_x1["mean_w", ]
  m2 <- moments_x2["mean_w", ]
  cov <- sum((x1 - m1) * (x2 - m2) * w)
  sd1 <- moments_x1["sd_w", ]
  sd2 <- moments_x2["sd_w", ]
  res <- unname(cov / (sd1 * sd2) / (n-1))
}

# .tau = function(x, y) {
#   acc <- 0
#   n <- length(as.vector(x))
# 
#   for (i in 2:n){
#     x_vec <- x[1:i-1]; y_vec <- y[1:i-1]
#     x_curr <- x[i]; y_curr <- y[i]
#     acc <- acc + sum(sign(x_vec-x_curr)*sign(y_vec-y_curr))
#   }
#   return (acc*2/(n*(n-1)))
# }

# # test
# library(SWIM)
# data("credit_data")
# credit_data <- credit_data[1:100,]
# stress.credit <- stress(type = "VaR", x = credit_data, k = "L", alpha = 0.9,
#                         q_ratio = 1.2)
# stress.credit <- stress(type = "VaR ES", x = stress.credit, k = "L", alpha = 0.9,
#                         q_ratio = 1.1, s = 2000)
# cor_stressed(stress.credit, xCol = c(1, 2), method="kendall")
# # cor(get_data(stress.credit)[, c(1,2)], method="kendall")

