#' Correlation of a Stressed Model
#' 
#' Provides the correlation of stressed model components
#'     (random variable) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM
#' @param method  Character, one of \code{"Pearson", "Spearman", "Kendall"}. (\code{default = "Pearson"}).
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
#' 
#' @seealso See \code{\link{var_stressed}} and \code{\link{sd_stressed}} compute
#'     stressed variance and standard deviations under the scenario weights, respectively.
#'          
#'     See \code{\link[stats]{cor}} for unweighted correlations between model components, while 
#'     \code{cor_stressed} return correlations between stressed model components
#'     
#' @export

cor_stressed <- function(object, xCol = c(1, 2), wCol = "all", method = "Pearson", base=FALSE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
  if (anyNA(object$x)) warning("x contains NA")
  if (!(method %in% c("Pearson", "Spearman", "Kendall"))) stop("Method must be one of Pearson, Spearman and Kendall")
  
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)
  
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- get_weights(object)[ ,wCol]  
  
  d <- ncol(x_data)
  n <- nrow(x_data)
  if (method == "Kendall" && (n > 1000 || d > 10)) {
    ans <- utils::menu(
      c("Yes", "No"), 
      title="The dataset is very large and calculating the weighted Kendall's tau might be time consuming. Do you want to proceed? Press 1 for yes and 2 to abort")
    if (ans != 1) invokeRestart("abort")
  }
  
  corr_w <- apply(X = as.matrix(new_weights), MARGIN = 2, 
                  FUN = .cor_helper, x_data = x_data, method = method)
  names(corr_w) <- names(object$specs)[wCol]
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    corr_base <- .cor_helper(x_data = x_data, w = old_weights, method = method)
    corr_w <- c(list("base" = corr_base), corr_w)
  }
  
  for (i in 1:length(corr_w)){
    colnames(corr_w[[i]]) <- cname
    rownames(corr_w[[i]]) <- cname
    diag(corr_w[[i]]) <- 1
  }
  return (corr_w)
}

.cor_helper <- function(x_data, w, method){
  d <- ncol(x_data)
  mat <- matrix(NA, nrow=d, ncol=d)

  for (i in 1:d){
    for (j in 1:d){
      mat[i,j] <- .corr(x_data[,c(i, j)], w, method)
    }
  }
  return (data.frame(mat))
} 

.corr <- function(x, w, method){
  x1 <- x[, 1]; x2 <- x[, 2]
  if (method == "Pearson") {
    res <- .Pearson(x1,x2,w)
  }
  if (method == "Kendall") {
    # res <- stats::cor(x1*w, x2*w, method = method)
    res <- .tau(x1, x2, w)
  }
  if (method == "Spearman") {
    x_rank <- rank(x1)
    y_rank <- rank(x2)
    res <- .Pearson(x_rank, y_rank, w)
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

.Pearson <- function(x1, x2, w){
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

.tau = function(x, y, w) {
  acc <- 0
  n <- length(as.vector(x))

  for (i in 2:n){
    x_js <- x[1:i-1]; y_js <- y[1:i-1]; w_js <- w[1:i-1]
    x_i <- x[i]; y_i <- y[i]; w_i <- w[i]
    acc <- acc + sum(sign(x_i-x_js) * sign(y_i-y_js) * w_js * w_i)
  }
  return (acc*2/(n*(n-1)))
}
