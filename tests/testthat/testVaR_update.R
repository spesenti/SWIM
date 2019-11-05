context("Stress VaR")
library("SWIM")

set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(100), 
  "gamma" = rgamma(100, shape = 2)))

################ test for update on stress_VaR ################

# two stresses
alpha <- c(0.95, 0.96)
k <- 1
q_old <- quantile(x[,1], probs = alpha, type = 1)
# next points after q_old in the order statistics
q_new <- c(sort(x[,1])[96] + 0.01, sort(x[,1])[97] + 0.01)

res1 <- stress_VaR(x, alpha = alpha, q = q_new)

# achieved stresses are not equal to the stressed quantiles provided
test_that("achieved VaR", {
  expect_message(stress_VaR(x, alpha = alpha, q = q_new))
  # the achieved quantiles are NOT the stressed quantiles
  expect_false(q_new[1] == as.numeric(quantile_stressed(res1, alpha, xCol = k))[1])
  expect_false(q_new[2] == as.numeric(quantile_stressed(res1, alpha, xCol = k))[2])
})

# the specs contain the achieved stresses
test_that("specs of achieved stresses", {
  expect_false(get_specs(res1)$q[1] == q_new[1])
  expect_false(get_specs(res1)$q[2] == q_new[2])
})


################ tests from testVaR.R ################

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k", "alpha", "q"))
  expect_equal(as.numeric(get_specs(res1)[1,2:3]), c(k, alpha[1]))
  expect_equal(as.numeric(get_specs(res1)[2,2:3]), c(k, alpha[2]))
  expect_equal(res1$type[[1]], "VaR")
  expect_equal(res1$type[[2]], "VaR")
  expect_type(get_weightsfun(res1), "list")
})

# Stress is fulfilled
test_that("stress", {
  # the probabiltiy is correct
  expect_equal(alpha[1], as.numeric(cdf(res1, xCol = 1, wCol = 1)(q_new)[1]))
  expect_equal(alpha[2], as.numeric(cdf(res1, xCol = 1, wCol = 2)(q_new)[2]))
})
