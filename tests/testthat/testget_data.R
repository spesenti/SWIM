context("Stress Get Data")
library("SWIM")

set.seed(0)
x <- data.frame(cbind(
  "normal" = rnorm(1000), 
  rgamma(1000, shape = 2), 
  rbeta(1000, shape1 = 2, shape2 = 2)))
colnames(x) <- c("normal", "", "beta")
colnames(x)
################ character colnames ################

k <- 1:3
new_means <- c(1, 1, 0.75)
res1 <- stress(type = "mean", x = x, k = k, new_means = new_means)

################ one stress ################


# get_data names test
test_that("get data", {
  # default
  expect_equal(colnames(get_data(res1)), colnames(x))
  # "all" 
  expect_equal(colnames(get_data(res1, xCol = "all")), colnames(x))
  # xCol numeric
  expect_equal(colnames(get_data(res1, xCol = colnames(x)[1])), colnames(x)[1])
  expect_equal(colnames(get_data(res1, xCol = 1)), colnames(x)[1])
  # xCol vector
  expect_equal(colnames(get_data(res1, xCol = colnames(x)[-2])), colnames(x)[-2])
  expect_equal(colnames(get_data(res1, xCol = c(1,3))), colnames(x)[-2])
  
  # errors: character and numeric
  expect_error(colnames(get_data(res1, xCol = c("all", "all"))))
  expect_error(colnames(get_data(res1, xCol = c(1, "all"))))
})

################ no colnames but data frame ################


set.seed(0)
x <- data.frame(cbind(
  rnorm(1000), 
  rgamma(1000, shape = 2), 
  rbeta(1000, shape1 = 2, shape2 = 2)))
colnames(x) <- c("X1", "X2", "X3")

k <- 1:3
new_means <- c(1, 1, 0.75)
res2 <- stress(type = "mean", x = x, k = k, new_means = new_means)



# get_data names test
test_that("get data", {
  # default
  expect_equal(colnames(get_data(res2)), colnames(x))
  # "all" 
  expect_equal(colnames(get_data(res2, xCol = "all")), colnames(x))
  # xCol numeric
  expect_equal(colnames(get_data(res2, xCol = colnames(x)[1])), colnames(x)[1])
  expect_equal(colnames(get_data(res2, xCol = 1)), colnames(x)[1])
  # xCol vector
  expect_equal(colnames(get_data(res2, xCol = colnames(x)[-2])), colnames(x)[-2])
  expect_equal(colnames(get_data(res2, xCol = c(1,3))), colnames(x)[-2])
  
  # errors: character and numeric
  expect_error(colnames(get_data(res2, xCol = c("all", "all"))))
  expect_error(colnames(get_data(res2, xCol = c(1, "all"))))
})


################ colnames == NULL ################


set.seed(0)
x <- as.matrix(cbind(
  rnorm(1000), 
  rgamma(1000, shape = 2), 
  rbeta(1000, shape1 = 2, shape2 = 2)))
colnames(x) <- c("X1", "X2", "X3")

k <- 1:3
new_means <- c(1, 1, 0.75)
res3 <- stress(type = "mean", x = x, k = k, new_means = new_means)



# get_data names test
test_that("get data", {
  # default
  expect_equal(colnames(get_data(res3)), colnames(x))
  # "all" 
  expect_equal(colnames(get_data(res3, xCol = "all")), colnames(x))
  # xCol numeric
  expect_equal(colnames(get_data(res3, xCol = 1)), "X1")
  # xCol vector
  expect_equal(colnames(get_data(res3, xCol = c(1,3))), c("X1", "X3"))
  
  # errors: character and numeric
  expect_error(colnames(get_data(res3, xCol = c("normal", "beta"))))
  expect_error(colnames(get_data(res3, xCol = c("all", "all"))))
  expect_error(colnames(get_data(res3, xCol = c(1, "all"))))
})


