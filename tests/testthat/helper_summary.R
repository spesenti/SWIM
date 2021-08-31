
sum_test <- function(res){
  w <- get_weights(res)
  w <- cbind(rep(1, nrow(w)), w)
  x <- get_data(res)
  sum_w <- NULL
  for( i in 1:(ncol(w))){
    sum_w <- c(sum_w, apply(x, 2, .moments, w = w[,i]))
  }
  sum <- unlist(summary(res, base = TRUE), use.names = FALSE)
  base_mean <- unlist(summary(res, base = TRUE)[[1]][1,], use.names = FALSE)
  test_that("weighted", {
     expect_equal(sum_w, sum)
     expect_equal(as.vector(colMeans(x)), base_mean)
  })
}

# .moments <- function(x, w){
#   n <- length(as.vector(x))
#   mean_w <- stats::weighted.mean(x = x, w = w)
#   sd_w <- sqrt(mean(w * (x - mean_w)^2) * n / (n-1)) 
#   skew_w <- mean(w * (x - mean_w)^3) / (sd_w^3) * n^2 / ((n-1) * (n-2))
#   ex_kurt_w <- mean(w * (x - mean_w)^4) / (sd_w^4) - 3
#   quartile_w <- as.matrix(Hmisc::wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75)))
#   moments_w <- rbind(mean_w, sd_w, skew_w, ex_kurt_w, quartile_w)
#   return(moments_w)
# }
