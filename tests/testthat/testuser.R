context("Stress user")
library("SWIM")

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2))) 

################ stress via function ################
new_weightsfun <- function(x)x^2
k <- 1
res1 <- stress(type = "user", x = x, new_weightsfun = new_weightsfun, k = k)

# output test
output_test(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c("type", "k"))
  expect_equal(res1$type[[1]], "user")
  expect_type(get_weightsfun(res1), "list")
})

################ stress via scenraio weights ################
new_weights <- cbind(exp(2 * sqrt(seq(1:nrow(x)))), rep(1, nrow(x)))
k <- 2
res2 <- stress(type = "user", x = x, new_weights = new_weights, k = k)

# output test
output_test(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c("type", "k"))
  expect_equal(res2$type[[1]], "user")
  expect_error(get_weightsfun(res2))
  expect_equal(dim(get_weights(res2))[2], 2)
})

################ merge two stresses ################
merge_test(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)