context("plot histogram")
library("SWIM")

################ stress ################

set.seed(0)
x <- data.frame("gamma" = rgamma(10^5, shape = 2))
res1 <- stress(type = "VaR", x = x, 
  alpha = c(0.75, 0.95), q_ratio = 1.1)
plot_h <- plot_hist(res1, xCol = 1, wCol = "all", base = TRUE, displ = FALSE)

################ plot sensitivity ################

test_that("output", {
  expect_named(plot_h, c( names(x), "stress", "value"))
  expect_true(all(levels(plot_h[,2]) %in% c("stress 1", "stress 2", "base")))
})  

test_that("value x", {
#base 1
  expect_equal(plot_h[plot_h$stress %in% "base", ][, 1], x[,1])
#stress 1
  expect_equal(plot_h[plot_h$stress %in% "stress 1", ][, 1], x[,1])
#stress 2
  expect_equal(plot_h[plot_h$stress %in% "stress 2", ][, 1], x[,1])
})

test_that("values", {
#base 1
  expect_equal(plot_h[plot_h$stress %in% "base", ][, 3], rep(1, nrow(x)))
#stress 1
  expect_equal(plot_h[plot_h$stress %in% "stress 1", ][, 3], get_weights(res1)[,1])
#stress 2
  expect_equal(plot_h[plot_h$stress %in% "stress 2", ][, 3], get_weights(res1)[,2])
})




