context("Sd")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR", x = x, 
               alpha = c(0.9, 0.95), q_ratio = 1.05)

s1 <- sd_stressed(res1, xCol = "all", wCol = "all", base = TRUE)
s2 <- sd_stressed(res1, xCol = "all", wCol = "all", base = TRUE)


################ stress ################
# output test
test_that("output", {
  expect_true(is.matrix(s1))
  expect_equal(colnames(s1), c("normal", "gamma"))
  expect_equal(rownames(s1), c("base", "stress 1", "stress 2"))
  expect_equal(dim(s1), c(3, 2))
  
  expect_true(is.matrix(s2))
  expect_equal(colnames(s2), c("normal", "gamma"))
  expect_equal(rownames(s2), c("base", "stress 1", "stress 2"))
  expect_equal(dim(s2), c(3, 2))
})

# Value test 
test_that("output", {
  expect_equal(s["base", "normal"], sd(x$normal))
  expect_equal(s["base", "gamma"], sd(x$gamma))
  
  expect_equal(s["base", "normal"], var(x$normal))
  expect_equal(s["base", "gamma"], var(x$gamma))
})