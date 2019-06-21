context("Stress VaR")
library("SWIM")

set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

################ one stress ################

alpha <- 0.9
q_ratio <- 1.05
k <- 1
res1 <- stress(type = "VaR", x = x, alpha = alpha, q_ratio = q_ratio, k = 1)

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k", "alpha", "q"))
  expect_equal(as.numeric(get_specs(res1)[2:3]), c(k, alpha))
  expect_equal(res1$type[[1]], "VaR")
  expect_type(get_weightsfun(res1), "list")
})

# Stress is fulfilled
test_that("stress", {
  expect_equal(alpha, as.numeric(cdf(res1, xCol = k, wCol = 1)(quantile(x[,k], alpha) * q_ratio)))
})

################ two stresses ################

alpha <- c(0.9, 0.95)
res2 <- stress(type = "VaR", x = x, alpha = alpha, q_ratio = q_ratio, k = 1)

# output test
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k", "alpha", "q"))
  expect_equal(as.numeric(get_specs(res2)[1,2:3]), c(k, alpha[1]))
  expect_equal(as.numeric(get_specs(res2)[2,2:3]), c(k, alpha[2]))
  expect_equal(res2$type[[1]], "VaR")
  expect_equal(res2$type[[2]], "VaR")
  expect_type(get_weightsfun(res2), "list")
  })

# Stress is fulfilled
test_that("stress", {
  expect_equal(alpha[1], as.numeric(cdf(res2, xCol = k, wCol = 1)(quantile(x[,k], alpha[1]) * q_ratio)))
  expect_equal(alpha[2], as.numeric(cdf(res2, xCol = k, wCol = 2)(quantile(x[,k], alpha[2]) * q_ratio)))
})


################ merge two stresses ################
merge_test(res1, res2)

################ summary ################

