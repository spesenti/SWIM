corr <- function(object, xCol = c(1, 2), wCol = 1, method = "pearson"){
  # if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  if (!(method %in% c("pearson", "spearman", "kendall"))) stop("Method must be one of pearson, spearman and kendall")
  
  new_weights <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  x1 <- x_data[, 1]; x2 <- x_data[, 2]
    
  if (method == "pearson") {
    res <- .pearson(x1,x2,new_weights)
  }
  if (method == "kendall") {
    # print(dim(x_data))
    # print(length(new_weights))
    # res <- cor(x_data*new_weights, method = method)
    res <- .tau(x1*new_weights, x2*new_weights)
  }
  if (method == "spearman") {
    x_rank <- rank(x1)
    y_rank <- rank(x2)
    res <- .pearson(x_rank, y_rank, new_weights)
  }  
  return (res)
}

# .moments <- function(x, w, degree=1){
#   return(mean(x^degree * w))
# }

.moments <- function(x, w){
  n <- length(as.vector(x))
  mean_w <- stats::weighted.mean(x = x, w = w)
  sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
  skew_w <- mean(w * (x - mean_w)^3) / (sd_w^3) * n^2 / ((n-1) * (n-2))
  ex_kurt_w <- mean(w * (x - mean_w)^4) / (sd_w^4) - 3
  quartile_w <- as.matrix(Hmisc::wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75)))
  moments_w <- rbind(mean_w, sd_w, skew_w, ex_kurt_w, quartile_w)
  return(moments_w)
}

.pearson <- function(x1,x2,new_weights){
  # m1 <- .moments(x1, new_weights)
  # m2 <- .moments(x2, new_weights)
  # print(length(m1))
  # print(length(x1))
  # 
  # cov <- .moments((x1 - m1) * (x2 - m2), new_weights)
  # var1 <- .moments(x1, new_weights, 2) - m1^2
  # var2 <- .moments(x2, new_weights, 2) - m2^2
  # res <- cov / sqrt(var1*var2)
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
    
    # xi <- x[i]; yi <- y[i]
    # for (j in 1:i-1){
    #   xj <- x[j]; yj <- y[j]
    #   if (is.logical(xi > xj & yi > yj) | is.logical(xi < xj & yi < yj))
    #     {acc <- acc+1}
    #   else {acc <- acc-1}
    #   }
  }
  return (acc*2/(n*(n-1)))
}

# test
library(SWIM)
data("credit_data")
credit_data <- credit_data[1:50,]
stress.credit <- stress(type = "VaR", x = credit_data, k = "L", alpha = 0.9,
                        q_ratio = 1.2)
corr(stress.credit, xCol = c(1, 2), method="kendall")

