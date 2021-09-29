context("SWIMw")
library("SWIM")

################ stress wass ################
set.seed(0)
x <- as.data.frame(cbind(
  "log.normal" = rlnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))

res1 <- stress_wass(type = "RM", x = x,
                   alpha = 0.9, q_ratio = 1.05)

################ cdf ################
grid <- seq(min(x$log.normal), max(x$log.normal), length.out = 5)
s1 <- cdf_stressed(res1, xCol = 1, wCol = "all", grid = grid, base = TRUE)

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

################ Sensitivity ################
f <- list(function(x)log(x), function(x)log(x))
k <- list(1,2)
s1 <- sensitivity(res1, wCol = 1, type = "all") 
s2 <- sensitivity(res1, wCol = 2, type = "all") 
## sensitivity of log-transformed data 
sf1 <- sensitivity(res1, wCol = 1, type = "all", f = f, k = k) 
sf2 <- sensitivity(res1, wCol = 2, type = "all", f = f, k = k) 
################ stress ################

# output test
test_that("output", {
  expect_named(s1, c("stress", "type", names(x)))
  expect_named(s2, c("stress", "type", names(x)))
  expect_named(sf1, c("stress", "type", names(x), c("f1", "f2")))
  expect_named(sf2, c("stress", "type", names(x), c("f1", "f2")))
  expect_true(all(s1[,1] %in% "stress 1"))
  expect_true(all(s2[,1] %in% "stress 2"))
  expect_true(all(sf1[,1] %in% "stress 1"))
  expect_true(all(sf2[,1] %in% "stress 2"))
  expect_true(all(levels(s1[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(s2[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(sf1[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(sf2[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
})

# sensitivity measures test
w <- get_weights(res1)
f_data <- cbind(f[[1]](x[,1]), f[[2]](x[,2]))

test_that("Gamma", {
  expect_equal(as.numeric(s1[1,3:4]), as.numeric(apply(x, MARGIN = 2, .gamma, w = w[,1])))
  expect_equal(as.numeric(s2[1,3:4]), as.numeric(apply(x, MARGIN = 2, .gamma, w = w[,2])))
  expect_equal(as.numeric(sf1[1,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .gamma, w = w[,1])))
  expect_equal(as.numeric(sf2[1,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .gamma, w = w[,2])))
})

test_that("Kolmogorov", {
  expect_equal(as.numeric(s1[2,3:4]), as.numeric(apply(x, MARGIN = 2, .kolmogorov, w = w[,1])))
  expect_equal(as.numeric(s2[2,3:4]), as.numeric(apply(x, MARGIN = 2, .kolmogorov, w = w[,2])))
  expect_equal(as.numeric(sf1[2,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .kolmogorov, w = w[,1])))
  expect_equal(as.numeric(sf2[2,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .kolmogorov, w = w[,2])))
})

test_that("Wasserstein", {
  expect_equal(as.numeric(s1[3,3:4]), as.numeric(apply(x, MARGIN = 2, .wasserstein, w = w[,1])))
  expect_equal(as.numeric(s2[3,3:4]), as.numeric(apply(x, MARGIN = 2, .wasserstein, w = w[,2])))
  expect_equal(as.numeric(sf1[3,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .wasserstein, w = w[,1])))
  expect_equal(as.numeric(sf2[3,3:6]), as.numeric(apply(cbind(x, f_data), MARGIN = 2, .wasserstein, w = w[,2])))
})


################ importance rank ################
im <- importance_rank(res1, wCol = 1:2, type = "all") 
imf <- importance_rank(res1, wCol = 1:2, type = "all", f = f, k = k) 

test_that("output", {
  expect_named(im, c("stress", "type", names(x)))
  expect_named(imf, c("stress", "type", names(x), c("f1", "f2")))
  expect_true(all(levels(im[,1]) %in% c("stress 1", "stress 2")))
  expect_true(all(levels(imf[,1]) %in% c("stress 1", "stress 2")))
  expect_true(all(levels(im[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(imf[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
})

test_that("ranks", {
  # Gamma
  expect_equal(as.numeric(rank(-s1[1, 3:4], ties.method = "min")), as.numeric(im[1,3:4]))
  expect_equal(as.numeric(rank(-s2[1, 3:4], ties.method = "min")), as.numeric(im[2,3:4]))
  expect_equal(as.numeric(rank(-sf1[1, 3:6], ties.method = "min")), as.numeric(imf[1,3:6]))
  expect_equal(as.numeric(rank(-sf2[1, 3:6], ties.method = "min")), as.numeric(imf[2,3:6]))
})

test_that("ranks", {
  # Wasserstein
  expect_equal(as.numeric(rank(-s1[3, 3:4], ties.method = "min")), as.numeric(im[3,3:4]))
  expect_equal(as.numeric(rank(-s2[3, 3:4], ties.method = "min")), as.numeric(im[4,3:4]))
  expect_equal(as.numeric(rank(-sf1[3, 3:6], ties.method = "min")), as.numeric(imf[3,3:6]))
  expect_equal(as.numeric(rank(-sf2[3, 3:6], ties.method = "min")), as.numeric(imf[4,3:6]))
})

################ plot sensitivity ################
plot_im <- plot_sensitivity(res1, wCol = 1:2, type = "all", displ = FALSE)
plot_imf <- plot_sensitivity(res1, wCol = 1:2, type = "all", f = f, k = k, displ = FALSE) 

test_that("output", {
  expect_named(plot_im, c("stress", "type", "X_all", "value"))
  expect_named(plot_imf, c("stress", "type", "X_all", "value"))
  expect_true(all(levels(plot_im[,1]) %in% c("stress 1", "stress 2")))
  expect_true(all(levels(plot_imf[,1]) %in% c("stress 1", "stress 2")))
  expect_true(all(levels(plot_im[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(plot_imf[,2]) %in% c("Gamma", "Kolmogorov", "Wasserstein")))
  expect_true(all(levels(plot_im[,3]) %in% names(x)))
  expect_true(all(levels(plot_imf[,3]) %in% c(names(x), "f1", "f2")))
})  

test_that("value", {
  #stress 1, Gamma
  expect_equal(as.numeric(s1[s1$type %in% "Gamma", ][,3:4]), plot_im[plot_im$stress %in% "stress 1" & plot_im$type %in% "Gamma", ][,4])
  #stress 2, Gamma
  expect_equal(as.numeric(s2[s2$type %in% "Gamma", ][,3:4]), plot_im[plot_im$stress %in% "stress 2" & plot_im$type %in% "Gamma", ][,4])
  #stress 1, Kolmogorov
  expect_equal(as.numeric(s1[s1$type %in% "Kolmogorov", ][,3:4]), plot_im[plot_im$stress %in% "stress 1" & plot_im$type %in% "Kolmogorov", ][,4])
  #stress 2, Kolmogorov
  expect_equal(as.numeric(s2[s2$type %in% "Kolmogorov", ][,3:4]), plot_im[plot_im$stress %in% "stress 2" & plot_im$type %in% "Kolmogorov", ][,4])
  #stress 1, Wasserstein
  expect_equal(as.numeric(s1[s1$type %in% "Wasserstein p = 1", ][,3:4]), plot_im[plot_im$stress %in% "stress 1" & plot_im$type %in% "Wasserstein p = 1", ][,4])
  #stress 2, Wasserstein
  expect_equal(as.numeric(s2[s2$type %in% "Wasserstein p = 1", ][,3:4]), plot_im[plot_im$stress %in% "stress 2" & plot_im$type %in% "Wasserstein p = 1", ][,4])
})

test_that("value function", {
  #stress 1, Gamma
  expect_equal(as.numeric(sf1[sf1$type %in% "Gamma", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 1" & plot_imf$type %in% "Gamma", ][,4])
  #stress 2, Gamma
  expect_equal(as.numeric(sf2[sf2$type %in% "Gamma", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 2" & plot_imf$type %in% "Gamma", ][,4])
  #stress 1, Kolmogorov
  expect_equal(as.numeric(sf1[sf1$type %in% "Kolmogorov", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 1" & plot_imf$type %in% "Kolmogorov", ][,4])
  #stress 2, Kolmogorov
  expect_equal(as.numeric(sf2[sf2$type %in% "Kolmogorov", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 2" & plot_imf$type %in% "Kolmogorov", ][,4])
  #stress 1, Wasserstein
  expect_equal(as.numeric(sf1[sf1$type %in% "Wasserstein p = 1", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 1" & plot_imf$type %in% "Wasserstein p = 1", ][,4])
  #stress 2, Wasserstein
  expect_equal(as.numeric(sf2[sf2$type %in% "Wasserstein p = 1", ][,3:6]), plot_imf[plot_imf$stress %in% "stress 2" & plot_imf$type %in% "Wasserstein p = 1", ][,4])
})

################ Correlation ################
s1 <- cor_stressed(res1, xCol = xCol, wCol = 1, method = "Pearson", base = TRUE)
test_that("output", {
  expect_true(is.list(s1))
  expect_named(s1, c("base", "stress 1"))
  
  expect_true(is.data.frame(s1$"stress 1"))
  expect_true(is.data.frame(s1$"base"))
  expect_equal(rep(length(xCol), 2), dim(s1$"stress 1"))
  expect_equal(rep(length(xCol), 2), dim(s1$"base"))
  
  expect_equal(rep(1, 2), unname(diag(data.matrix(s1$"stress 1"))))
  expect_equal(rep(1, 2), unname(diag(data.matrix(s1$"base"))))
  
  expect_equal(cor(x[, xCol]), data.matrix(s1$"base"))
})

################ mean ################
s <- mean_stressed(res1, xCol = "all", wCol = "all", base = TRUE)

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


################ sd ################
s1 <- sd_stressed(res1, xCol = "all", wCol = "all", base = TRUE)

# output test
test_that("output", {
  expect_true(is.matrix(s1))
  expect_equal(colnames(s1), c("normal", "gamma"))
  expect_equal(rownames(s1), c("base", "stress 1", "stress 2"))
  expect_equal(dim(s1), c(3, 2))
})

# Value test 
test_that("output", {
  expect_equal(s1["base", "normal"], sd(x$normal))
  expect_equal(s1["base", "gamma"], sd(x$gamma))
})

################ ES ################
alpha <- c(0.8, 0.9)
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1
levels <- seq(0.1, 0.9, by = 0.1)

.ES.stressed1 <- ES_stressed(res1, alpha = levels, xCol = k, wCol = 1, base = TRUE)

test_that("names", {
  expect_equal(colnames(.ES.stressed1), colnames(.ES.stressed2))
  expect_true(all(colnames(.ES.stressed1) == c(colnames(x)[k], paste("base", colnames(x)[k]))))
  expect_equal(rownames(.ES.stressed1), paste(100 * levels, "%", sep = ""))
})

################# checking ES for x[,1] #################
x1 <- x[, 1]

.VaR.stressed1 <- VaR_stressed(res1, alpha = levels, xCol = k, wCol = 1, base = TRUE)
w <- get_weights(res1)

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))

for(i in 1:length(levels)){
  ES1[i] <- mean(w[, 1] * (x1 - .VaR.stressed1[i, 1]) * (x1 > .VaR.stressed1[i, 1])) / 
    (1 - levels[i]) + .VaR.stressed1[i, 1]
}

test_that("stressed", {
  # test values for the stressed ES 
  # expect_equal(as.numeric(.ES.stressed1[, 1]), ES1)
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.stressed1)))
})

################# checking ES for x[, 2] #################
x2 <- x[, 2]

.VaR.stressed1 <- VaR_stressed(res, alpha = levels, xCol = 2, wCol = 1, base = FALSE)
w <- get_weights(res)

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))

for(i in 1:length(levels)){
  ES1[i] <- mean(w[, 1] * (x2 - .VaR.stressed1[i]) * (x2 > .VaR.stressed1[i])) / 
    (1 - levels[i]) + .VaR.stressed1[i]
}

.ES.stressed1 <- ES_stressed(res, alpha = levels, xCol = 2, wCol = 1, base = FALSE)

test_that("stressed", {
  # test values for the stressed ES 
  # expect_equal(as.numeric(.ES.stressed1[, 1]), ES1)
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.stressed1)))
})

################# checking ES for base #################
x <- x

.VaR.base <- VaR_stressed(res, alpha = levels, xCol = "all", wCol = 1, base = TRUE)[, 3:4]

# ES of the model component with the first stress
ES1 <- rep( 0, length(levels))

for(i in 1:length(levels)){
  ES1[i] <- mean((x[, 1] - .VaR.base[i, 1]) * (x[, 1] > .VaR.base[i, 1])) / 
    (1 - levels[i]) + .VaR.base[i, 1]
}

.ES.stressed1 <- ES_stressed(res, alpha = levels, xCol = "all", wCol = 1, base = TRUE)[, 3:4]

test_that("stressed", {
  # test values of stressed ES are larger than values of stressed VaR
  expect_true(all(as.numeric(.ES.stressed1) > as.numeric(.VaR.base)))
})

################ VaR ################
alpha <- c(0.8, 0.9)
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1
prob <- seq(0.1, 0.9, by = 0.1)

.VaR.stressed1 <- VaR_stressed(res1, alpha = prob, xCol = k, wCol = 1, base = TRUE)
q_base <- as.numeric(quantile(x[, k], probs = prob, type = 1))
q_stressed1 <- quantile_stressed(res1, probs = prob, xCol = k, wCol = 1, type = "i/n")  

test_that("base", {
  # test that the values for base is equal to the R quantile function 
  expect_true(all(abs(as.numeric(.VaR.stressed1[, 2]) - q_base) < 0.1))
  # test that the names are correct
  expect_true(all(colnames(.VaR.stressed1) == c(names(x)[k], "base normal")))
})

test_that("stressed", {
  # test that the values for the stressed VaR is equal to 
  # the quantile_stressed function
  expect_equal(as.numeric(.VaR.stressed1[, 1]), as.numeric(q_stressed1))
  # test that the names are correct
  expect_true(all(colnames(.VaR.stressed1)[1] == colnames(q_stressed1)))
})


################ quantile ################
prob <- seq(0.1, 0.9, by = 0.1)
alpha <- c(0.8, 0.9)
q_ratio <- 1.05
s_ratio <- 1.1
k <- 1

test_that("quantile", {
  # test that stress_quantile is the same as the wtd.quantile in Hmics package
  for(k in c("quantile","(i-1)/(n-1)", "i/(n+1)","i/n")){
    for(i in 1:ncol(get_weights(res1))){
      # xCol = 1
      q_stressed1 <- as.numeric(quantile_stressed(res1, prob, xCol = 1, wCol = i, type = k))
      discrete.quantile <- as.numeric(Hmisc::wtd.quantile(x[, 1], weights = get_weights(res1)[, i], prob, type = k))
      expect_true(all(abs(q_stressed1 - discrete.quantile) < 0.1))
      # xCol = 2
      q_stressed1 <- as.numeric(quantile_stressed(res1, prob, xCol = 2, wCol = i, type = k))
      discrete.quantile <- as.numeric(Hmisc::wtd.quantile(x[, 2], weights = get_weights(res1)[, i], prob, type = k))
      expect_true(all(abs(q_stressed1 - discrete.quantile) < 0.1))
    }
  }
})