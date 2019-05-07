# method reporting stressed quantiles

# x         object
# xCol   integer, vector, columns of x$x, (default = "all")  
# wCol   integer, columns of new_weights, (default = 1)
# type      type = c('i/n','(i-1)/(n-1)','i/(n+1)', 'quantile'), which interpolation of quantiles, see Hmisc

quantile.SWIM <- function(x, p, xCol = "all", wCol = 1, type = c('i/n','(i-1)/(n-1)','i/(n+1)', 'quantile')){
  if(!is.SWIM(x)) stop("wrong object")
  if(missing(type)) type <- as.character('quantile')
  if(anyNA(x$x)) warning("'x' contains NA")
  new_weights <-get.weights(x)[,wCol]
  if(is.character(xCol) && xCol == "all") xCol <- 1:dim(get.data(x))[2]
  if(is.null(colnames(get.data(x)))){
    cname <-  paste("X", as.character(xCol), sep = "")
  }else{
    cname <- colnames(get.data(x))[xCol]
  } 
  x_data <- as.matrix(get.data(x)[, xCol])

  require(Hmisc, quietly = TRUE)
  quantile_w <- as.matrix(apply(X = as.matrix(x_data), FUN = wtd.quantile, MARGIN = 2, weights = new_weights, probs = p, type = type))
  if(length(p) == 1 && length(cname) > 1) quantile_w <- matrix(quantile_w, nrow = 1)
  colnames(quantile_w) <- cname
  rownames(quantile_w) <- paste(p * 100, "%", sep = "")
  return(quantile_w)
  }


