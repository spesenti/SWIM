context("Sensitivity")
library("SWIM")

################ stress ################
set.seed(0)
x <- as.data.frame(cbind(
  "log.normal" = rlnorm(1000), 
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR", x = x, alpha = c(0.9, 0.95), q_ratio = 1.05)

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
