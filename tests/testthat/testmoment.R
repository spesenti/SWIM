context("Stress moment")
library("SWIM")

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2), 
  "beta" = rbeta(1000, shape1 = 2, shape2 = 2)))

################ one stress ################
k <- list(1, 2, c(1, 2))
f <-  list(function(x)x, function(x)x, function(x)x[1] * x[2])
m <- c(0, 2, 0.5)
res1 <- stress_moment(x = x, f = f, k = k, m = m, 
  method = "Newton", control = list(maxit = 1000, ftol = 1E-10))

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k"))
  expect_equal(res1$type[[1]], "moment")
  expect_error(get_weightsfun(res1))
})

# Stress is fulfilled
test_that("stress", {
  w <- get_weights(res1)
  expect_equal(mean(w * x[,k[[1]]]), m[[1]])
  expect_equal(mean(w * x[,k[[2]]]), m[[2]])
  expect_equal(mean(w * x[,k[[3]][1]] * x[,k[[3]][2]]), m[[3]])
})


################ stress intervals ################

k <- list(1, 3)
f <- list(function(x)(x > 1.5), function(x)(x > 0.9))
m <-  c(0.9, 0.9)
res2 <- stress_moment(x = x, f = f, k = k, m = m)

# output test
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k"))
  expect_equal(res2$type[[1]], "moment")
  expect_error(get_weightsfun(res2))
})

# Stress is fulfilled
test_that("stress", {
  w <- get_weights(res2)
  expect_equal(mean(w * f[[1]](x[,k[[1]]])), m[[1]])
  expect_equal(mean(w * f[[2]](x[,k[[2]]])), m[[2]])
})

################ merge two stresses ################
merge_test(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)