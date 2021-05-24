#' Correlation a Stressed Model
#' 
#' Provides the correlation of two stressed model components
#'     (random variable) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM
#' @param method  Character, one of \code{"pearson", "spearman", "kendall"}. Pearson by default.
#' 
#' @return The correlation coefficient of the \code{xCol}
#'     components of the stressed model with weights \code{wCol} and method \code{wCol}.
#' 
#' @details \code{cor_stressed}: The correlation coefficient of two 
#'      chosen stressed model components, subject to the calculated scenario weights.
#'      The scorrelation coefficient of a stressed model component
#'      is denoted as \deqn{cor^W}
#'
#'      The function \code{cor_stressed} provides stressed correlation coefficient of model
#'      components with different interpolations.
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
#' cor_stressed(res1, xCol = c(1, 2), wCol = 1)
#' ## baseline correlation
#' cor(x$normal, x$gamma)
#' 
#' @author Kent Wu
#' @describeIn cor_stressed correlation coefficient of stressed model components
#' 
#' @seealso See \code{\link{stress_moment}} stressing a baseline 
#'     model with desired moment constraints, and \code{\link{var_stressed}} provides
#'     stressed variances under the scenario weights
#' @export


cor_stressed <- function(object, xCol = c(1, 2), wCol = "all", method = "pearson", base=TRUE){
  # if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  if (!(method %in% c("pearson", "spearman", "kendall"))) stop("Method must be one of pearson, spearman and kendall")
  
  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- get_weights(object)[ ,wCol]  
  
  corr_w <- apply(X = as.matrix(new_weights), MARGIN = 2, 
                  FUN = .helper, x_data = x_data, cname = cname, method = method)
  names(corr_w) <- paste("stress", wCol)
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    corr_base <- .helper(x_data = x_data, cname = cname, new_weights = old_weights, method = method)
    corr_w <- c(list("base" = corr_base), corr_w)
  }
  return (corr_w)
}

.helper <- function(x_data, cname, new_weights, method){
  # print(length(new_weights))
  # .temp <- function(y, w, method) apply(X = w, FUN = .corr, MARGIN = 2, y = x_data, method = method)
  corr_w <- .corr(x_data, new_weights, method = method)
  colnames(corr_w) <- cname
  rownames(corr_w) <- cname
  return(data.frame(corr_w))
} 

.corr <- function(x, w, method){
  x1 <- x[, 1]; x2 <- x[, 2]
  if (method == "pearson") {
    res <- .pearson(x1,x2,w)
  }
  if (method == "kendall") {
    # res <- cor(x1*w, x2*w, method = method)
    res <- .tau(x1*w, x2*w)
  }
  if (method == "spearman") {
    x_rank <- rank(x1)
    y_rank <- rank(x2)
    res <- .pearson(x_rank, y_rank, w)
  }
  return (matrix(c(1, res, res, 1), byrow = TRUE, nrow = 2))
}

.moments <- function(x, w){
  n <- length(as.vector(x))
  mean_w <- stats::weighted.mean(x = x, w = w)
  sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
  moments_w <- rbind(mean_w, sd_w)
  return(moments_w)
}

.pearson <- function(x1,x2,new_weights){
  moments_x1 <- .moments(x1, new_weights)
  moments_x2 <- .moments(x2, new_weights)
  m1 <- moments_x1["mean_w", ]
  m2 <- moments_x2["mean_w", ]
  cov <- .moments((x1 - m1) * (x2 - m2), new_weights)["mean_w", ]
  sd1 <- moments_x1["sd_w", ]
  sd2 <- moments_x2["sd_w", ]
  res <- unname(cov / (sd1 * sd2))
}

.tau = function(x, y) {
  acc <- 0
  n <- length(as.vector(x))

  for (i in 2:n){
    x_vec <- x[1:i-1]; y_vec <- y[1:i-1]
    x_curr <- x[i]; y_curr <- y[i]
    acc <- acc + sum(sign(x_vec-x_curr)*sign(y_vec-y_curr))
  }
  return (acc*2/(n*(n-1)))
}

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

