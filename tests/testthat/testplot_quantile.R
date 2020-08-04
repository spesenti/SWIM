context("plot quantile")
library("SWIM")

################ stress ################

set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(10^5), 
  "gamma" = rgamma(10^5, shape = 2)))
res1 <- stress(type = "VaR", x = x, alpha = c(0.75, 0.95), q_ratio = 1.15)
# X1
p_quant1 <- plot_quantile(res1, xCol = 1, wCol = 1:2, base = TRUE, displ = FALSE)
# X2
res2 <- stress(type = "VaR", x = x, alpha = c(0.75, 0.95), q_ratio = 1.15, k = 2)
p_quant2 <- plot_quantile(res2, xCol = 2, wCol = 1:2, base = TRUE, displ = FALSE)
grid <- seq(0, 1, length.out = 500)
################ plot sensitivity ################

test_that("output", {
  expect_named(p_quant1, c( "grid", "stress", "value"))
  expect_named(p_quant2, c( "grid", "stress", "value"))
  expect_true(all(levels(p_quant1[,2]) %in% c("stress 1", "stress 2", "base")))
  expect_true(all(levels(p_quant2[,2]) %in% c("stress 1", "stress 2", "base")))
})  

test_that("value x", {
# X1  
  #base 
  expect_equal(p_quant1[p_quant1$stress %in% "base", ][, 3], 
               as.numeric(quantile(get_data(res1)[, 1], probs = grid)))
  #stress 1
  expect_equal(p_quant1[p_quant1$stress %in% "stress 1", ][, 3], 
               as.numeric(quantile_stressed(res1, probs = grid, xCol = 1, wCol = 1)))
  #stress 2
  expect_equal(p_quant1[p_quant1$stress %in% "stress 2", ][, 3], 
               as.numeric(quantile_stressed(res1, probs = grid, xCol = 1, wCol = 2)))
# X2  
  #base 
  expect_equal(p_quant2[p_quant2$stress %in% "base", ][, 3], 
               as.numeric(quantile(get_data(res2)[, 2], probs = grid)))
  #stress 1
  expect_equal(p_quant2[p_quant2$stress %in% "stress 1", ][, 3], 
               as.numeric(quantile_stressed(res2, probs = grid, xCol = 2, wCol = 1)))
  #stress 2
  expect_equal(p_quant2[p_quant2$stress %in% "stress 2", ][, 3], 
               as.numeric(quantile_stressed(res2, probs = grid, xCol = 2, wCol = 2)))
})

# based on stat_ecdf of ggplot2
test_that("grid", {
# X1
    #base
    expect_equal(p_quant1[p_quant1$stress %in% "base", ][, 1], grid)
    #stress 1
    expect_equal(p_quant1[p_quant1$stress %in% "stress 1", ][, 1], grid)
    #stress 2
    expect_equal(p_quant1[p_quant1$stress %in% "stress 2", ][, 1], grid)
    
# X2
  #base
  expect_equal(p_quant2[p_quant2$stress %in% "base", ][, 1], grid)
  #stress 1
  expect_equal(p_quant2[p_quant2$stress %in% "stress 1", ][, 1], grid)
  #stress 2
  expect_equal(p_quant2[p_quant2$stress %in% "stress 2", ][, 1], grid)
})
