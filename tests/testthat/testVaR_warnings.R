context("Stress VaR Warnings")
library("SWIM")

# this test is to proivde warnings when there is not enough data points
### NOTE: this test assumes that stress_VaR and stress_ES start from a 
###       Monte Carlo sample, that is each data point has equal weight.


# for stress_VaR we need that the is NO dta point between VaR and q.

x <- 1:10
alpha <- 0.5
old_q <- quantile(x,alpha,type = 1)


################ stressing VaR up - WARNING ################
new_q <- 5.5
# stress VaR from 5 to 5.5 - same probability

test_that("VaR_warning", {
  expect_error(stress_VaR(x,alpha, q = new_q))
})

################ stressing VaR down - NO WARNING ################

new_q <- 4.9
# stress VaR from 5 to 4.9 - different probability

test_that("VaR_warning", {
  expect_condition(stress_VaR(x,alpha, q = new_q, regexp = NA))
})

################ NOT stressing VaR - NO WARNING ################

new_q <- old_q
# not stress VaR

test_that("VaR_warning", {
  expect_condition(stress_VaR(x,alpha, q = new_q, regexp = NA))
})
