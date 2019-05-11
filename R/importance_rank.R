# Function to calculate ranking of a SWIM object input

# x         object
# xCol      integer vector, columns of x$x (default = "all")  
# wCol      integer vector, columns of new_weights, (default = "all")
# type      character vector, c("Gamma", "Wasserstein", "all"). The Kolmogorov distance is the same for all inputs.
# f         list of functions, calcualted the sensitivity of the transformed input vector. List needs to have the same length as xCol. 

  importance_rank <- function(x, xCol = "all", wCol = "all", 
    type = c("Gamma", "Wasserstein", "all"), f = NULL){
   if (!is.SWIM(x)) stop("Wrong object")
   if (anyNA(x$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
  
   sens_w <- sensitivity(x, xCol = xCol, wCol = wCol, type = type, f = f)
   if (length(sens_w) < 4) stop("Only one input provided.")
   rank_w <- t(apply(X = sens_w[ , 1:(length(sens_w) - 2)], MARGIN = 1, FUN = function(z) rank(z, ties.method = "min")))
   rank_w <- cbind(rank_w, sens_w[ , (length(sens_w) - 1):length(sens_w)])
    
   if (is.character(type) && type == "all") rank_w <- rank_w[-which(rank_w[,"type"] == "Kolmogorov"), ]
   return(rank_w)
  }