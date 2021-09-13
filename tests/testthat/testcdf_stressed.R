context("Multi-weight cdf")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "log.normal" = rlnorm(1000), 
  "gamma" = rgamma(1000, shape = 2),
  "normal" = rnorm(1000)))

res <- stress(type = "VaR", x = x, alpha = 0.9, q_ratio = 1.2)
grid <- seq(min(x$normal), max(x$normal), length.out = 5)
s1 <- cdf_stressed(res, xCol = 1, wCol = "all", grid = grid, base = TRUE)

################ stress ################
# format test
test_that("output", {
  expect_true(is.matrix(s1))
  expect_equal(rownames(s1), c("stress 1", "base"))
  })

# value test 
test_that("output", {
  colnames(s1) <- NULL # remove col names to compare 
  expect_equal(stats::ecdf(x$log.normal)(grid), s1["base", ])
  }
)
