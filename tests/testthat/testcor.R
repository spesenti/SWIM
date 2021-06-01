context("Correaltion")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "log.normal" = rlnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

res <- stress(type = "VaR", x = x, alpha = 0.9, q_ratio = 1.2)
res <- stress(type = "VaR", x = res, alpha = 0.95, q_ratio = 1.05)

xCol = c(1, 2)

s1 <- cor_stressed(res, xCol = xCol, wCol = 1, method = "pearson", base = TRUE)
s2 <- cor_stressed(res, xCol = xCol, wCol = "all", method = "kendall", base = TRUE)
s3 <- cor_stressed(res, xCol = xCol, wCol = "all", method = "spearman", base = FALSE)

################ stress ################
# output test
test_that("output", {
  # expect_warning(cor_stressed(x, xCol = xCol, wCol = 1, method = "peason"), 
  #                "Method must be one of pearson, spearman and kendall")

  expect_true(is.list(s1))
  expect_true(is.list(s2))
  expect_true(is.list(s3))

  expect_named(s1, c("base", "stress 1"))
  expect_named(s2, c("base", "stress 1", "stress 2"))
  expect_named(s3, c("stress 1", "stress 2"))
  
  })

# model test 
test_that("output", {
  expect_true(is.data.frame(s1$"stress 1"))
  expect_true(is.data.frame(s1$"base"))
  expect_equal(rep(length(xCol), 2), dim(s1$"stress 1"))
  expect_equal(rep(length(xCol), 2), dim(s1$"base"))
  
  expect_true(is.data.frame(s2$"stress 1"))
  expect_true(is.data.frame(s2$"stress 2"))
  expect_true(is.data.frame(s2$"base"))
  expect_equal(rep(length(xCol), 2), dim(s2$"stress 1"))
  expect_equal(rep(length(xCol), 2), dim(s2$"stress 2"))
  expect_equal(rep(length(xCol), 2), dim(s2$"base"))

  expect_true(is.data.frame(s3$"stress 1"))
  expect_true(is.data.frame(s3$"stress 2"))
  expect_equal(rep(length(xCol), 2), dim(s3$"stress 1"))
  expect_equal(rep(length(xCol), 2), dim(s3$"stress 2"))
  
  # Check self variation
  expect_equal(rep(1, 2), unname(diag(data.matrix(s1$"stress 1"))))
  expect_equal(rep(1, 2), unname(diag(data.matrix(s1$"base"))))
  
  expect_equal(rep(1, 2), unname(diag(data.matrix(s2$"stress 1"))))
  expect_equal(rep(1, 2), unname(diag(data.matrix(s2$"stress 2"))))
  expect_equal(rep(1, 2), unname(diag(data.matrix(s2$"base"))))
  
  expect_equal(rep(1, 2), unname(diag(data.matrix(s3$"stress 1"))))
  expect_equal(rep(1, 2), unname(diag(data.matrix(s3$"stress 2"))))
  
  # check values
  w <- get_weights(res)
  w1 <- w[, 1]
  w2 <- w[, 2]
  expect_true(all(data.matrix(s1$"stress 1") == .corr(x, w1, "pearson")))
  expect_true(all(data.matrix(s2$"stress 1") == .corr(x, w1, "kendall")))
  expect_true(all(data.matrix(s3$"stress 1") == .corr(x, w1, "spearman")))
  
  })
