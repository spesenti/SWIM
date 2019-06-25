context("Stress mean")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

alpha <- c(0.8, 0.9)
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1
res <- stress(type = "VaR ES", x = x, alpha = alpha, q_ratio = q_ratio, 
  s_ratio = s_ratio, k = k)
res1 <- stress(type = "user", x = res, new_weights = rep(1, nrow(x)))
################ quantile ################
prob <- seq(0.1, 0.9, by = 0.1)

test_that("quantile", {
# test that stress_quantile is the same as the wtd.quantile in Hmics package
for(k in c("quantile","(i-1)/(n-1)", "i/(n+1)","i/n")){
    for(i in 1:ncol(get_weights(res1))){
# xCol = 1
    expect_equal(as.numeric(quantile_stressed(res1, prob, xCol = 1, wCol = i,
      type = k)), as.numeric(Hmisc::wtd.quantile(x[, 1], 
        weights = get_weights(res1)[, i], prob, type = k)))  
# xCol = 2
    expect_equal(as.numeric(quantile_stressed(res1, prob, xCol = 2, wCol = i,
      type = k)), as.numeric(Hmisc::wtd.quantile(x[, 2], 
        weights = get_weights(res1)[, i], prob, type = k)))
    }
  }
})

