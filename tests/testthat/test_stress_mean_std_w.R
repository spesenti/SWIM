context("Stressing Wass with Mean and Standard Deviation")
library("SWIM")

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2))) 

################ stress via function ################
res1 <- stress_wass(type = "mean sd", x = x, k = 1,
                    new_mean=1, new_sd=0.9)

# output test
output_test_w(res1, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res1), c('type', 'k'))
  expect_equal(res1$type[[1]], "mean sd")
  expect_type(res1$h, "list")
  expect_type(res1$lam, "list")
  expect_type(res1$str_fY, "list")
  expect_type(res1$str_FY, "list")
  expect_type(res1$str_FY_inv, "list")
  expect_type(res1$gamma, "list")
})

################ stress via scenraio weights ################
res2 <- stress_mean_std_w(x = x, 
                          new_mean=2.2, new_sd=1.5, k = 2)

# output test
output_test_w(res2, x)

# specs test
test_that("specs", {
  expect_named(get_specs(res2), c('type', 'k'))
  expect_equal(res2$type[[1]], "mean sd")
  expect_type(res2$h, "list")
  expect_type(res2$lam, "list")
  expect_type(res2$str_fY, "list")
  expect_type(res2$str_FY, "list")
  expect_type(res2$str_FY_inv, "list")
  expect_type(res2$gamma, "list")
})

################ merge two stresses ################
merge_test_w(res1, res2)

################ summary ################
sum_test(res1)
sum_test(res2)