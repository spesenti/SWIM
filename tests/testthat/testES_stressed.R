context("VaR stressed")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

alpha <- c(0.8, 0.9)
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1
res <- stress(type = "VaR ES", x = x, alpha = alpha, q_ratio = q_ratio, 
              s_ratio = s_ratio, k = k)

################ ES ################
levels <- seq(0.1, 0.9, by = 0.1)

.ES.stressed1 <- ES_stressed(res, alpha = levels, xCol = k, wCol = 1, base = TRUE)
.ES.stressed2 <- ES_stressed(res, alpha = levels, xCol = k, wCol = 2, base = TRUE)

test_that("names", {
    expect_equal(colnames(.ES.stressed1), colnames(.ES.stressed2))
    expect_equal(colnames(.ES.stressed1), c(colnames(x)[k], paste("base", colnames(x)[k])))
    expect_equal(rownames(.ES.stressed1), paste(100 * levels, "%", sep = ""))
})

################# checking ES for x[,1] #################
x1 <- x[, 1]

.VaR.stressed1 <- VaR_stressed(res, alpha = levels, xCol = k, wCol = 1, base = TRUE)
.VaR.stressed2 <- VaR_stressed(res, alpha = levels, xCol = k, wCol = 2, base = TRUE)
w <- get_weights(res)

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))
# ES of the model component with the second stress
ES2 <- rep( 0, length(levels))
for(i in 1:length(levels)){
    ES1[i] <- mean(w[, 1] * (x1 - .VaR.stressed1[i, 1]) * (x1 > .VaR.stressed1[i, 1])) / 
        (1 - levels[i]) + .VaR.stressed1[i, 1]
    ES2[i] <- mean(w[, 2] * (x1 - .VaR.stressed2[i, 1]) * (x1 > .VaR.stressed2[i, 1])) / 
        (1 - levels[i]) + .VaR.stressed2[i, 1]
}

test_that("stressed", {
  # test values for the stressed ES 
  expect_equal(as.numeric(.ES.stressed1[, 1]), ES1)
  expect_equal(as.numeric(.ES.stressed2[, 1]), ES2)
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.stressed1)))
  expect_true(all(as.numeric(.ES.stressed2) > as.numeric(.VaR.stressed2)))
  })

################# checking ES for x[, 2] #################
x2 <- x[, 2]

.VaR.stressed1 <- VaR_stressed(res, alpha = levels, xCol = 2, wCol = 1, base = FALSE)
.VaR.stressed2 <- VaR_stressed(res, alpha = levels, xCol = 2, wCol = 2, base = FALSE)
w <- get_weights(res)

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))
# ES of the model component with the second stress
ES2 <- rep( 0, length(levels))
for(i in 1:length(levels)){
  ES1[i] <- mean(w[, 1] * (x2 - .VaR.stressed1[i]) * (x2 > .VaR.stressed1[i])) / 
    (1 - levels[i]) + .VaR.stressed1[i]
  ES2[i] <- mean(w[, 2] * (x2 - .VaR.stressed2[i]) * (x2 > .VaR.stressed2[i])) / 
    (1 - levels[i]) + .VaR.stressed2[i]
}

.ES.stressed1 <- ES_stressed(res, alpha = levels, xCol = 2, wCol = 1, base = FALSE)
.ES.stressed2 <- ES_stressed(res, alpha = levels, xCol = 2, wCol = 2, base = TRUE)


test_that("stressed", {
  # test values for the stressed ES 
  expect_equal(as.numeric(.ES.stressed1[, 1]), ES1)
  expect_equal(as.numeric(.ES.stressed2[, 1]), ES2)
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.stressed1)))
  expect_true(all(as.numeric(.ES.stressed2) > as.numeric(.VaR.stressed2)))
})



################# checking ES for base #################
x <- x

.VaR.base <- VaR_stressed(res, alpha = levels, xCol = "all", wCol = 1, base = TRUE)[, 3:4]

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))
# ES of the model component with the second stress
ES2 <- rep( 0, length(levels))
for(i in 1:length(levels)){
  ES1[i] <- mean((x[, 1] - .VaR.base[i, 1]) * (x[, 1] > .VaR.base[i, 1])) / 
    (1 - levels[i]) + .VaR.base[i, 1]
  ES2[i] <- mean((x[, 2] - .VaR.base[i, 2]) * (x[, 2] > .VaR.base[i, 2])) / 
    (1 - levels[i]) + .VaR.base[i, 2]
}

.ES.stressed1 <- ES_stressed(res, alpha = levels, xCol = "all", wCol = 1, base = TRUE)[, 3:4]
.ES.stressed2 <- ES_stressed(res, alpha = levels, xCol = "all", wCol = 2, base = TRUE)[, 3:4]


test_that("stressed", {
  # test values for the stressed ES 
  expect_equal(as.numeric(.ES.stressed1[, 1]), ES1)
  expect_equal(as.numeric(.ES.stressed2[, 1]), ES1)
  expect_equal(as.numeric(.ES.stressed1[, 2]), ES2)
  expect_equal(as.numeric(.ES.stressed2[, 2]), ES2)
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.base)))
  expect_true(all(as.numeric(.ES.stressed2) > as.numeric(.VaR.base)))
})


