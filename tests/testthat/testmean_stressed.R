context("Mean")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR", x = x, 
               alpha = c(0.9, 0.95), q_ratio = 1.05)

s <- mean_stressed(res1, xCol = "all", wCol = "all", base = TRUE)


################ stress ################
# output test
test_that("output", {
  expect_true(is.matrix(s))
  expect_equal(colnames(s), c("normal", "gamma"))
  expect_equal(rownames(s), c("base", "stress 1", "stress 2"))
  expect_equal(dim(s), c(3, 2))
})

# Value test 
test_that("output", {
  expect_equal(s["base", "normal"], mean(x$normal))
  expect_equal(s["base", "gamma"], mean(x$gamma))
})