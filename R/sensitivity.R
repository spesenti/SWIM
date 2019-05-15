#' Sensitivity Measures of a \code{SWIM} Object
#' 
#' Provides sensitivity measures of an object of class \code{SWIM}.
#'      
#' @inheritParams summary.SWIM
#' @param type    Character, one of \code{"Gamma", "Kolmogorov", 
#'                "Wasserstein", "all"}.
#' @param f       List of functions, same length as \code{xCol}. If 
#'                provided, the sensitivity measures of the transformed
#'                data of the \code{SWIM} object is returned
#'                (\code{default = NULL}).
#' 
#' @details Sensitivity measures, comparing the data and the stressed 
#'     data of the \code{SWIM} object are calculated. 
#'     
#'     \code{"Gamma"}, the \emph{Reverse Sensitivity Measure}, defined 
#'     for a random variable \code{X} and scenario weights \code{w} by     
#'     \deqn{"Gamma" = ( E(X * w) - E(X) ) / normalised,}
#'     where the normalisation is such that \code{|"Gamma"| <= 1}, see
#'     \insertCite{Pesenti2019reverse}{SWIM}. Loosely speaking the 
#'     Reverse Sensitivity Measure is the normalised difference between
#'     the first moments of the stressed and the baseline distribution
#'     of \code{X}, suitably normalised.
#'     
#'     \code{"Kolmogorov"}, the Kolmogorov distance, defined for 
#'     distribution functions \code{F, G} by 
#'     \deqn{sup |F(x) - G(x)|.}
#'     Note that the Kolmogorov distance of one stress is the same for 
#'     all inputs. Should be used to compare different stresses.   
#'     
#'     \code{"Wasserstein"}, the Wasserstein distance of order 1, defined
#'     for two distribution functions \code{F, G} by 
#'     \deqn{\int | F(x) - G(x)| dx.} 
#' 
#' @return A data.frame containting the sensitivity measures of the 
#'     data and the stressed data of the \code{SWIM} object.The last 
#'     two rows display the \code{stress} and the \code{type} of the 
#'     sensitivity measure. 
#'     
#' @references \insertRef{Pesenti2019reverse}{SWIM}
#'  
#' @export
#' 

  sensitivity <- function(object, xCol = "all", wCol = "all", type = c("Gamma",      "Kolmogorov", "Wasserstein", "all"), f = NULL){
   if (!is.SWIM(object)) stop("Wrong object")
   if (anyNA(object$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
   if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get.data(object))
   if (is.null(colnames(get.data(object)))){
    cname <-  paste("X", as.character(xCol), sep = "")
   } else {
    cname <- colnames(get.data(object))[xCol]
   } 
   x_data <- get.data(object)[ , xCol]
   if (!is.null(f)){
    for(i in 1:length(xCol)){
      x_data[, i] <- sapply(x_data[, i], f[[i]])
    }
   }
  
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(object))
   new_weights <- get.weights(object)[ , wCol]  
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