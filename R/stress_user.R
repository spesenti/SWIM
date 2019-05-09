## User defined stress

## INPUTS: 
## x            vector, matrix, data frame - realisations of a random or SWIM object
## new_weights  numeric, matrix, data frame - weights. If new_weights has dim bigger than 2, every colum is considered a vector of weights that form 1 stress. 
## k            numeric - column of x that are stressed (default = 1)

stress_user <- function(x, new_weights, k = 1){
  if(is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  if(anyNA(x_data)) warning("'x' contains NA")
  if(is.null(colnames(x_data))) colnames(x_data) <- paste("X", 1 : ncol(x_data), sep = "")
  
  if(is.data.frame(new_weights) | is.vector(new_weights)) new_weights <- as.matrix(new_weights)

  if(is.function(new_weights)) new_weights <- as.list(new_weights)
  if(is.list(new_weights)) new_weights <- sapply(new_weights, function(f)f(x[, k]))
  if(any(new_weights < 0)) stop("invalid 'new_weights' argument")
  new_weights <- t(t(new_weights) / colMeans(new_weights))
  max_length <- ncol(new_weights)
  colnames(new_weights) <- paste("stress", 1 : max_length)
  
  specs <- data.frame("type" = rep("user", length.out = max_length), "k" = rep(k, length.out = max_length), constr = "user", stringsAsFactors = FALSE)
  rownames(specs) <- paste("stress", 1 : max_length)
  my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
  if(is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }