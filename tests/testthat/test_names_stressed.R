context("Stress names")
library("SWIM")

################ Stress Var ################
x <- as.data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR", x = x,
               alpha = 0.9, q_ratio = 1.05, names = "A")

# specs test
test_that("specs", {
  expect_equal(names(res1$specs), "A")
  expect_equal(colnames(get_weights(res1)), "A")
})

################ Stress Moment ################
set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2),
  "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))

## stressing covariance of columns 1, 2 while leaving the means unchanged
res1 <- stress_moment(x = x,
                      f = list(function(x)x, function(x)x, function(x)x[1] * x[2]),
                      k = list(1, 2, c(1, 2)), m = c(0, 2, 0.5),
                      method = "Newton", control = list(maxit = 1000, ftol = 1E-10), names = "A")

# specs test
test_that("specs", {
  expect_equal(names(res1$specs), c("A"))
  expect_equal(colnames(get_weights(res1)),c("A"))
})

################ Stress User ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "user", x = x, new_weightsfun = function(x)x ^ 2, k = 1, names = "A")

# specs test
test_that("specs", {
  expect_equal(names(res1$specs), c("A"))
  expect_equal(colnames(get_weights(res1)),c("A"))
})

################ Stress VaR ES ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR ES", x = x,
               alpha = c(0.9, 0.95), q_ratio = 1.05, s_ratio = 1.08, names = c("A", "B"))

## calling stress_VaR_ES directly
## stressing "gamma"
res2 <- stress_VaR_ES(x = res1, alpha = 0.9,
                      q_ratio = 1.03, s_ratio = c(1.05, 1.08), k = 2, names = c("C", "D"))

# specs test
test_that("specs", {
  expect_equal(names(res2$specs), c("A", "B", "C", "D"))
  expect_equal(colnames(get_weights(res2)), c("A", "B", "C", "D"))
})