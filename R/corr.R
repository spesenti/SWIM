corr <- function(object, xCol = c(1, 2), wCol = 1, method){
  # if (!is.SWIM(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  
  new_weights <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  res <- cor(t(t(x_data)*new_weights), method = method)
  return (res)
}

# # test
# data("credit_data")
# credit_data <- credit_data[1:500,]
# stress.credit <- stress(type = "VaR", x = credit_data, k = "L", alpha = 0.9,
#                         q_ratio = 1.2)
# corr(stress.credit, xCol = c(1, 2), method="pearson")

