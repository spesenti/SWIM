# Function to calculate sensitivity measures of a SWIM object

# x         object
# xCol      integer vector, columns of x$x (default = "all")  
# wCol      integer vector, columns of new_weights, (default = "all")
# type      character vector, type = c("Gamma", "Kolmogorov", "Wasserstein", "all"). The Kolmogorov distance is the same for all x
# f         list of functions, calcualted the sensitivity of the transformed input vector. List needs to have the same length as xCol. 

  sensitivity <- function(x, xCol = "all", wCol = "all", type = c("Gamma",      "Kolmogorov", "Wasserstein", "all"), f = NULL){
   if (!is.SWIM(x)) stop("Wrong object")
   if (anyNA(x$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
   if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get.data(x))
   if (is.null(colnames(get.data(x)))){
    cname <-  paste("X", as.character(xCol), sep = "")
   } else {
    cname <- colnames(get.data(x))[xCol]
   } 
   x_data <- get.data(x)[ , xCol]
   if (!is.null(f)){
    for(i in 1:length(xCol)){
      x_data[, i] <- sapply(x_data[, i], f[[i]])
    }
   }
  
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(x))
   new_weights <- get.weights(x)[ , wCol]  
   sens_w <- setNames(data.frame(matrix(ncol = length(xCol) + 2, nrow = 0)), c(cname, "stress", "type"))
   if (type == "Gamma" || type == "all"){
    sens_gamma_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .gamma, z = z)
    sens_gw <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_gamma_w)
    if (length(wCol) == 1) sens_gw <- as.matrix(t(sens_gw))
    if (length(xCol) == 1) colnames(sens_gw) <- cname
    sens_w <- rbind(sens_w, data.frame(sens_gw, stress = paste("stress", wCol, sep = " "), type = rep("Gamma", length.out = length(wCol))))
   }
  
   if (type == "Kolmogorov" || type == "all"){
    sens_kolmogorov_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .kolmogorov, z = z)
    sens_kw <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_kolmogorov_w)
    if (length(wCol) == 1) sens_kw <- as.matrix(t(sens_kw))
    if (length(xCol) == 1) colnames(sens_kw) <- cname
    sens_w <- rbind(sens_w, data.frame(sens_kw, stress = paste("stress", wCol, sep = " "), type = rep("Kolmogorov", length.out = length(wCol))))
   }
  
   if (type == "Wasserstein" || type == "all"){
    sens_wasser_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .wasserstein, z = z)
    sens_ww <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_wasser_w)
    if (length(wCol) == 1) sens_ww <- as.matrix(t(sens_ww))
    if (length(xCol) == 1) colnames(sens_ww) <- cname
    sens_w <- rbind(sens_w, data.frame(sens_ww, stress = paste("stress", wCol, sep = " "), type = rep("Wasserstein", length.out = length(wCol))))
    }
   rownames(sens_w) <- NULL
   return(sens_w)
  }



 # help function Reverse Sensitivity, Gamma
 # comparison between input vectors for a given stress
  .gamma <- function(z, w){
   w <- as.numeric(w)
   w_comm <- sort(w)[rank(z, ties.method = "first")]
   w_counter <- sort(w, decreasing = TRUE)[rank(z, ties.method = "first")]
   if (cov(z, w) >= 0){
    gamma_sens <- cov(z, w) / cov(z, w_comm)
   } else {
    gamma_sens <- - cov(z, w) / cov(z, w_counter)
   }
   return(gamma_sens)
  }

 # help function Kolmogorov distance
 # maximal difference between the corresponding ecdf
 # comparison between different stresses. All inputs from one stress have   the same Kolmogorov distance. 
  .kolmogorov <- function(z, w){
   n <- length(z)
   xw_cdf <- cumsum(w[rank(sort(z))]) 
   kol_sense <- max(abs(xw_cdf - 1:n)) / n
   return(kol_sense)
  }

 # help function Wasserstein distance of order p = 1 
 # x   vector
 # w   vector of weights

  .wasserstein <- function(z, w, p = 1){
   n <- length(z)
   x_sort <- sort(z)
   w_cdf <- cumsum(w[rank(x_sort)])[1:(n - 1)] 
   x_diff <- diff(x_sort, lag = 1)
   wasser_sens <- sum(abs(w_cdf - 2:n) * x_diff) / n
   return(wasser_sens)
  }