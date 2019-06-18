output_test <- function(res, x){
  test_that("output",{
  
 ## Is of class SWIM
  expect_s3_class(res, "SWIM")
 ## Scenario weights
  w <- get_weights(res)
  expect_equal(as.numeric(colMeans(w)), rep(1, ncol(w)))
  expect_true(all(w > 0))
 ## data
  expect_true(all(get_data(res) == x))
})
}
