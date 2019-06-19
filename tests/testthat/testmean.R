context("Stress mean")
library("SWIM")

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2), 
  "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))

################ one stress ################
k <- 1:3
new_means <- c(1, 1, 0.75)
res1 <- stress(type = "mean", x = x, k = k, new_means = new_means)

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k"))
  expect_equal(res1$type[[1]], "mean")
  expect_error(get_weightsfun(res1))
})

# Stress is fulfilled
test_that("stress", {
  w <- get_weights(res1)
  expect_equal(as.numeric(colMeans(w * x[,k])), new_means)
})
