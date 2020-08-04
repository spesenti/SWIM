context("plot weights")
library("SWIM")

################ stress ################

data("credit_data")

# X1
res1 <- stress_VaR(credit_data, alpha = c(0.9, 0.95), q_ratio = 1.1, k =1) 

p_weights1 <- plot_weights(res1, xCol = "L", wCol = 1:2, n = "all", displ = FALSE)
# X2
res2 <- stress_VaR_ES(res1, alpha = 0.9, q_ratio = 1.1, s_ratio = 1.2, k =1) 
p_weights2 <- plot_weights(res2, xCol = 1, wCol = "all", n = "all", displ = FALSE)
################ plot sensitivity ################

test_that("output", {
  expect_named(p_weights1, c( "L", "stress", "value"))
  expect_named(p_weights2, c( "L", "stress", "value"))
  expect_true(all(levels(p_weights1[,2]) %in% c("stress 1", "stress 2")))
  expect_true(all(levels(p_weights2[,2]) %in% c("stress 1", "stress 2", "stress 3")))
})  

test_that("value x", {
# X1  
  #stress 1
  expect_equal(p_weights1[p_weights1$stress %in% "stress 1", ][, "value"], 
               get_weights(res1)[, "stress 1"])
  #stress 2
  expect_equal(p_weights1[p_weights1$stress %in% "stress 2", ][, "value"], 
               get_weights(res1)[, "stress 2"])
  # X2  
  #stress 1
  expect_equal(p_weights2[p_weights2$stress %in% "stress 1", ][, "value"], 
               get_weights(res2)[, "stress 1"])
  #stress 2
  expect_equal(p_weights2[p_weights2$stress %in% "stress 2", ][, "value"], 
               get_weights(res2)[, "stress 2"])
  #stress 3
  expect_equal(p_weights2[p_weights2$stress %in% "stress 3", ][, "value"], 
               get_weights(res2)[, "stress 3"])
})

test_that("L", {
# X1
    #stress 1
    expect_equal(p_weights1[p_weights1$stress %in% "stress 1", ][, "L"], 
                 as.numeric(get_data(res1, xCol = "L")))
    #stress 2
    expect_equal(p_weights1[p_weights1$stress %in% "stress 2", ][, "L"], 
               as.numeric(get_data(res1, xCol = "L")))
  
# X2
  #stress 1
  expect_equal(p_weights2[p_weights2$stress %in% "stress 1", ][, "L"], 
               as.numeric(get_data(res2, xCol = "L")))
  #stress 2
  expect_equal(p_weights2[p_weights2$stress %in% "stress 2", ][, "L"], 
               as.numeric(get_data(res2, xCol = "L")))
  #stress 3
  expect_equal(p_weights2[p_weights2$stress %in% "stress 3", ][, "L"], 
               as.numeric(get_data(res2, xCol = "L")))
  
})
