output_test <- function(res, x){
  test_that("output",{
  
 ## of class SWIM
  expect_s3_class(res, "SWIM")
 ## scenario weights
  w <- get_weights(res)
  expect_equal(as.numeric(colMeans(w)), rep(1, ncol(w)))
  expect_true(all(w > 0))
  ## data
  expect_true(all(get_data(res) == x))
  })
  }

# testing the merge.SWIM function
merge_test <- function(res1, res2){
  res <- merge(res1, res2)
  output_test(res, get_data(res))
}


output_test_w <- function(res, x){
  test_that("output",{
    
    ## of class SWIM
    expect_s3_class(res, "SWIMw")
    ## scenario weights
    w <- get_weights(res)
    expect_equal(as.numeric(colMeans(w)), rep(1, ncol(w)))
    expect_true(all(w > 0))
    ## data
    expect_true(all(get_data(res) == x))
  })
}

# testing the merge.SWIM function
merge_test_w <- function(res1, res2){
  res <- merge(res1, res2)
  output_test_w(res, get_data(res))
}