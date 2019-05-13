#' Summarizing a SWIM object
#' 
#' This funcion is a \code{\link[utils]{methods}} for an object of class 
#'     \code{SWIM}.
#'     
#' @inheritParams get.data
#' @param xCol    Integer vector, columns of \code{x} for which the summary
#'                should be returned, (\code{default = "all"}) 
#' @param wCol    Integer vector, columns of \code{new_weights} for which
#'                the summary should be returned, (\code{default = "all"})  
#' @param base    Logical, if \code{TRUE} the summary of the baseline is
#'                returned (\code{default = FALSE}).
#' @details  
#' 
#' @return \code{summary.SWIM} returns a list where each components
#'     correponds to a different stress. Every component contains the
#'     summary statistics of the colums of the data of the \code{SWIM}
#'     object, including:
#'     \tabular{ll}{
#'       \code{mean}        \cr
#'       \code{sd}          \cr
#'       \code{skewness}    \cr
#'       \code{ex kurtosis} \cr
#'       \code{1Qrt}        \cr
#'       \code{Median}      \cr
#'       \code{3Qrt}        \cr
#'     } 
#' 
#' @author Silvana M. Pesenti 
#' 
#' @seealso \code{\link{summary}}, \code{\link{SWIM}}
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
   rownames(moments_W) <- c("mean", "sd", "skewness", "ex kurtosis", "1Qrt", "Median", "3Qrt")
   return(data.frame(moments_W))
  } 

 # help function calculates weighted moments, mean, sd, skewness, excess kurtosis, quartiles
 # x   numeric vector of observations 
 # w   numeric vector of weights

  .moments <- function(x, w){
   n <- length(as.vector(x))
   mean_w <- weighted.mean(x = x, w = w)
   sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
   skew_w <- mean(w * (x - mean_w)^3) / (sd_w^3) * n^2 / ((n-1) * (n-2))
   ex_kurt_w <- mean(w * (x - mean_w)^4) / (sd_w^4) - 3
   quartile_w <- as.matrix(Hmisc::wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75)))
   moments_w <- rbind(mean_w, sd_w, skew_w, ex_kurt_w, quartile_w)
   return(moments_w)
  }

