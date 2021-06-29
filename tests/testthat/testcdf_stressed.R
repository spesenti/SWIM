context("Multi-weight cdf")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "log.normal" = rlnorm(1000), 
  "gamma" = rgamma(1000, shape = 2),
  "normal" = rnorm(1000)))

res <- stress(type = "VaR", x = x, alpha = 0.9, q_ratio = 1.2)
g1 <- cbind(seq(min(x$log.normal), max(x$log.normal), length.out = 5),
            seq(min(x$gamma), max(x$gamma), length.out = 5),
            seq(min(x$normal), max(x$normal), length.out = 5))
g2 <- seq(min(x$normal), max(x$normal), length.out = 5)
s1 <- cdf_stressed(res, xCol = "all", wCol = 1, grid = g1, base = TRUE)
s2 <- cdf_stressed(res, xCol = 3, wCol = 1, grid = g2)


################ stress ################
# format test
test_that("output", {
  expect_true(is.matrix(s1))
  expect_true(is.matrix(s2))
  
  expect_equal(colnames(s1), c("log.normal", "gamma", "normal", "base log.normal", "base gamma", "base normal"))
  expect_equal(colnames(s2), "normal")
  
  expect_equal(c(5,6), dim(s1))
  expect_equal(c(5,1), dim(s2))
  })

# value test 
test_that("output", {
  expect_equal(stats::ecdf(x$log.normal)(g1[, 1]), s1[, 4])
  expect_equal(stats::ecdf(x$gamma)(g1[, 2]), s1[, 5])
  expect_equal(stats::ecdf(x$normal)(g1[, 3]), s1[, 6])
  }
)
