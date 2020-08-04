context("plot cdf")
library("SWIM")

################ stress ################

set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(10^5), 
  "gamma" = rgamma(10^5, shape = 2)))
res1 <- stress(type = "VaR", x = x, alpha = c(0.75, 0.95), q_ratio = 1.15)
# X1
p_cdf1 <- plot_cdf(res1, xCol = 1, wCol = 1:2, base = TRUE, displ = FALSE)
# X2
p_cdf2 <- plot_cdf(res1, xCol = 2, wCol = 1:2, base = TRUE, displ = FALSE)

################ plot sensitivity ################

test_that("output", {
  expect_named(p_cdf1, c( names(x)[1], "stress", "value"))
  expect_named(p_cdf2, c( names(x)[2], "stress", "value"))
  expect_true(all(levels(p_cdf1[,2]) %in% c("stress 1", "stress 2", "base")))
  expect_true(all(levels(p_cdf2[,2]) %in% c("stress 1", "stress 2", "base")))
})  

test_that("value x", {
# X1  
  #base 1
  expect_equal(p_cdf1[p_cdf1$stress %in% "base", ][, 1], x[,1])
  #stress 1
  expect_equal(p_cdf1[p_cdf1$stress %in% "stress 1", ][, 1], x[,1])
  #stress 2
  expect_equal(p_cdf1[p_cdf1$stress %in% "stress 2", ][, 1], x[,1])
# X2  
  #base 1
  expect_equal(p_cdf2[p_cdf2$stress %in% "base", ][, 1], x[,2])
  #stress 1
  expect_equal(p_cdf2[p_cdf2$stress %in% "stress 1", ][, 1], x[,2])
  #stress 2
  expect_equal(p_cdf2[p_cdf2$stress %in% "stress 2", ][, 1], x[,2])
})

# based on stat_ecdf of ggplot2
test_that("values", {
# X1
    #base
    expect_equal(p_cdf1[p_cdf1$stress %in% "base", ][, 3], rep(1, nrow(x)))
    #stress 1
    expect_equal(p_cdf1[p_cdf1$stress %in% "stress 1", ][, 3], get_weights(res1)[,1])
    #stress 2
    expect_equal(p_cdf1[p_cdf1$stress %in% "stress 2", ][, 3], get_weights(res1)[,2])

# X2
  #base
  expect_equal(p_cdf2[p_cdf2$stress %in% "base", ][, 3], rep(1, nrow(x)))
  #stress 1
  expect_equal(p_cdf2[p_cdf2$stress %in% "stress 1", ][, 3], get_weights(res1)[,1])
  #stress 2
  expect_equal(p_cdf2[p_cdf2$stress %in% "stress 2", ][, 3], get_weights(res1)[,2])
})
