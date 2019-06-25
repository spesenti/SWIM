context("Stress prob")
library("SWIM")

set.seed(0)
x <- data.frame("normal" = rnorm(1000))

################ one interval ################
prob1 <- 0.008
upper1 <- -2.4
k <- 1
res1 <- stress(type = "prob", x = x, prob = prob1, upper = upper1, k = k)

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k", "prob", "upper", "lower"))
  expect_equal(as.numeric(get_specs(res1)[2:4]), c(k, prob1, upper1))
  expect_equal(res1$type[[1]], "prob")
  expect_type(get_weightsfun(res1), "list")
})

# Stress is fulfilled
test_that("stress", {
  expect_equal(cdf(res1, xCol = 1)(-2.4), prob1)
})


################ two intervals ################
prob2 <- c(0.008, 0.06)
lower2 <- c(-3, -2)
upper2 <- c(-2.4, -1.6)
res2 <- stress_prob(x = x, prob = prob2, lower = lower2, upper = upper2)

# tests output
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k", "prob1", "prob2", "upper1", "upper2", "lower1", "lower2"))
  expect_equal(as.numeric(get_specs(res2)[-1]), c(k, prob2, upper2, lower2))
  expect_equal(res2$type[[1]], "prob")
  expect_type(get_weightsfun(res2), "list")
})

# Stress is fulfilled
test_that("stress", {
  expect_equal(cdf(res2, xCol = 1)(c(-2.4, -1.6)) - cdf(res2, xCol = 1)(c(-3, -2)), prob2)
})

################ merge two stresses ################
merge_test(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)