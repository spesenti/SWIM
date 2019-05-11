#' Summarizing a SWIM object
#' 
#' This funcion is a \code{\link[utils]{methods}} for a class 
#'     \code{SWIM} object.
#'     
#' @inheritParams stress_VaR
#' @param xCol    Integer vector, columns of \code{x} for which the summary
#'                should be returned, (\code{default = "all"}) 
#' @param wCol    Integer vector, columns of \code{new_weights} for which
#'                the summary should be returned, (\code{default = "all"})  
#' @param base    Logical, if \code{TRUE} the summary of baseline is
#'                returned.
#' @details  
#' 
#' @return A list... 
#' 
#' @author Silvana M. Pesenti 
#' 
#' @seealso summary
#' @export
#' 

summary.SWIM <- function(x, xCol = "all", wCol = "all", base = FALSE){
  if (!is.SWIM(x)) stop("Wrong object")
  if (anyNA(x$x)) warning("x contains NA")
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get.data(x))
  if (is.null(colnames(get.data(x)))){
    cname <-  paste("X", as.character(xCol), sep = "")
  } else {
    cname <- colnames(get.data(x))[xCol]
  } 
  x_data <- as.matrix(get.data(x)[, xCol])
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(x))
  new_weights <- get.weights(x)[ ,wCol]  

  summary_w <- apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .summary, x_data = x_data, cname = cname, base = base)

  if (base == TRUE){
  old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
  summary_base <- .summary(x_data = x_data, cname = cname, new_weights = old_weights)
  summary_w[["base"]] <- summary_base
  }
  if (length(summary_w) == 1) names(summary_w) <- paste("stress", wCol)
    return(summary_w)
  }

 # help function, calculates summary when new_weights is a vector 
  .summary <- function(x_data, cname, new_weights, base){
   .temp <- function(y, w) apply(X = y, FUN = .moments, MARGIN = 2, w = w)
   moments_W <- .temp(x_data, new_weights)
   colnames(moments_W) <- cname
   rownames(moments_W) <- c("mean", "SD", "skewness", "ex kurtosis",  paste("quartile", c("25%", "50%", "75%")))
   return(data.frame(moments_W))
  } 

 # help function calculates weighted moments, mean, sd, skewness, excess kurtosis, quartiles
 # x   numeric vector of observations 
 # w   numeric vector of weights

  .moments <- function(x, w){
   require(Hmisc, quietly = TRUE)
   n <- length(as.vector(x))
   mean_w <- weighted.mean(x = x, w = w)
   sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
   skew_w <- mean(w * (x - mean_w)^3) / (sd_w^3) * n^2 / ((n-1) * (n-2))
   ex_kurt_w <- mean(w * (x - mean_w)^4) / (sd_w^4) - 3
   quartile_w <- as.matrix(wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75)))
   moments_w <- rbind(mean_w, sd_w, skew_w, ex_kurt_w, quartile_w)
   return(moments_w)
  }

