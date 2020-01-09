context("Stress VaR ES Warnings")
library("SWIM")

x <- 1:10
alpha <- 0.5
old_q <- quantile(x,alpha,type = 1)
old_ES <- old_q+1/(1-alpha)*mean(pmax(x-old_q,0))


# this test is to proivde warnings when there is not enough data points

################ stressing VaR up and ES down - WARNING ################

new_q <- 6
# stress VaR from 5 to 6
new_ES <- 7
# stress ES from 8 to 7

test_that("VaR_ES_warning", {
  expect_error(stress_VaR_ES(x,alpha, q = new_q, s = new_ES))
})

################ stressing VaR up and ES up - WARNING ################

new_q <- 8
# stress VaR from 5 to 6
new_ES <- 9
# stress ES from 8 to 9

test_that("VaR_ES_warning", {
  expect_error(stress_VaR_ES(x,alpha, q = new_q, s = new_ES))
})

################ stressing VaR down and ES down - WARNING ################

new_q <- 4.5
# stress VaR from 5 to 4
new_ES <- 5
# stress ES from 8 to 7

test_that("VaR_ES_warning", {
  expect_error(stress_VaR_ES(x,alpha, q = new_q, s = new_ES))
})

################ Not stressing VaR, stressing ES - WARNING ################
# equal values

new_q <- 5
# not stress VaR
new_ES <- 5.5
# stress ES from 8 to 5.5

test_that("VaR_ES_warning", {
  expect_error(stress_VaR_ES(x,alpha, q = new_q, s = new_ES))
})

################ Not stressing VaR, stressing ES - NO WARNING ################
# equal values

new_q <- 5
# not stress VaR
new_ES <- 9
# stress ES from 8 to 

test_that("VaR_ES_warning", {
  expect_condition(stress_VaR_ES(x,alpha, q = new_q, s = new_ES), regexp = NA)
})

################ Not stressing VaR, Not stressing ES - NO WARNING ################
# equal values

new_q <- old_q
# not stress VaR
new_ES <- old_ES
# not stress ES 

test_that("VaR_ES_warning", {
  expect_condition(stress_VaR_ES(x,alpha, q = new_q, s = new_ES), regexp = NA)
})

################ stressing VaR, Not stressing ES - NO WARNING ################
# equal values

new_q <- 6
# not stress VaR
new_ES <- old_ES
# not stress ES 

test_that("VaR_ES_warning", {
  expect_condition(stress_VaR_ES(x,alpha, q = new_q, s = new_ES), regexp = NA)
})

