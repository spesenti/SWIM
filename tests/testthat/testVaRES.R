context("Stress VaR ES")
library("SWIM")

set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

################ one stress ################

alpha <- 0.9
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1
res1 <- stress(type = "VaR ES", x = x, alpha = alpha, q_ratio = q_ratio, 
        s_ratio = s_ratio, k = 1)

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k", "alpha", "q", "s"))
  expect_equal(as.numeric(get_specs(res1)[2:3]), c(k, alpha))
  expect_equal(res1$type[[1]], "VaR ES")
  expect_type(get_weightsfun(res1), "list")
})

# Stress is fulfilled
test_that("stress", {
  #VaR is the 90th quantile
  VaR <- quantile(x[,k], alpha)
  expect_equal(alpha, as.numeric(cdf(res1, xCol = k, wCol = 1)(VaR * q_ratio)))
  #ES is the correct
  ES <- mean((get_data(res1)[, k] - VaR) * (get_data(res1)[, k] > VaR)) / (1 - alpha) + VaR
  ES_stressed <- mean(get_weights(res1) * (get_data(res1)[, k] - VaR * q_ratio) * (get_data(res1)[, k] > VaR * q_ratio)) / (1 - alpha) + VaR  * q_ratio
  expect_equal(ES * s_ratio, ES_stressed)
})
  
################ two stresses ################

alpha <- c(0.8, 0.9)
k <- 1
res2 <- stress(type = "VaR ES", x = x, alpha = alpha, q_ratio = q_ratio, 
  s_ratio = s_ratio, k = k)

# output test
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k", "alpha", "q", "s"))
  expect_equal(as.numeric(get_specs(res2)[1,2:3]), c(k, alpha[1]))
  expect_equal(as.numeric(get_specs(res2)[2,2:3]), c(k, alpha[2]))
  expect_equal(res2$type[[1]], "VaR ES")
  expect_equal(res2$type[[2]], "VaR ES")
  expect_type(get_weightsfun(res2), "list")
})

# Stress is fulfilled
test_that("stress", {
 # Stressed VaR is correct 
  VaR <- quantile(x[,k], alpha)
  expect_equal(alpha[1], as.numeric(cdf(res2, xCol = k, wCol = 1)(VaR[1] * q_ratio)))
  expect_equal(alpha[2], as.numeric(cdf(res2, xCol = k, wCol = 2)(VaR[2] * q_ratio)))
 # Stress 1: ES is the correct
  x_data <- get_data(res2)[, k]
  ES <- mean((x_data - VaR[1]) * (x_data > VaR[1])) / (1 - alpha[1]) + VaR[1]
  ES_stressed <- mean(get_weights(res2)[, 1] * (x_data - VaR[1] * q_ratio) * (x_data > VaR[1] * q_ratio)) / (1 - alpha[1]) + VaR[1]  * q_ratio
  expect_equal(as.numeric(ES) * s_ratio, as.numeric(ES_stressed))

 # Stress 2: ES is the correct
  ES <- mean((x_data - VaR[2]) * (x_data > VaR[2])) / (1 - alpha[2]) + VaR[2]
  ES_stressed <- mean(get_weights(res2)[, 2] * (x_data - VaR[2] * q_ratio) * (x_data > VaR[2] * q_ratio)) / (1 - alpha[2]) + VaR[2]  * q_ratio
  expect_equal(as.numeric(ES) * s_ratio, as.numeric(ES_stressed))
})

################ merge two stresses ################
merge_test(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)