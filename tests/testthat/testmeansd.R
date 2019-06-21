context("Stress mean sd")
library("SWIM")
requireNamespace("Weighted.Desc.Stat", quietly = TRUE)

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2), 
  "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))

################ one stress ################
k <- 1
new_means <- 0.1
new_sd <- 1.1

res1 <- stress(type = "mean sd", x = x, k = k, new_means = new_means, 
  new_sd = new_sd, method = "Newton", control = list(maxit = 1000, ftol = 1E-15))

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k"))
  expect_equal(res1$type[[1]], "mean sd")
  expect_error(get_weightsfun(res1))
})

# Stress is fulfilled
test_that("stress", {
  w <- get_weights(res1)
  expect_equal(Weighted.Desc.Stat::w.mean(x[,k], w), new_means)
  expect_equal(Weighted.Desc.Stat::w.sd(x[,k], w), new_sd)
  })

################ stressing two colums ################
k <- 1:2
new_means <- c(0.1, 2.5)
new_sd <- c(1.1, 1.6)

res2 <- stress(type = "mean sd", x = x, k = k, new_means = new_means, 
  new_sd = new_sd, method = "Newton", control = list(maxit = 1000, ftol = 1E-15))

# output test
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k"))
  expect_equal(res2$type[[1]], "mean sd")
  expect_error(get_weightsfun(res2))
})

# Stress is fulfilled
test_that("stress", {
  w <- get_weights(res2)
  expect_equal(as.numeric(colMeans(w * x[,k])), new_means)
  expect_equal(Weighted.Desc.Stat::w.sd(x[,k[1]], w), new_sd[1])
  expect_equal(Weighted.Desc.Stat::w.sd(x[,k[2]], w), new_sd[2])
})

################ merge two stresses ################
merge_test(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)