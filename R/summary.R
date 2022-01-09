#' Summarising Stressed Models
#' 
#' This function is a \code{\link[utils]{methods}} for an object of class 
#'     \code{SWIM} or \code{SWIMw}. Provides summary statistics of the stochastic model, 
#'     stressed using the scenario weights. 
#'     
#' @inheritParams get_data
#' @param ...     Additional arguments will be ignored. 
#' @param xCol    Numeric or character vector, (names of) the columns of 
#'                the underlying data 
#'                of the \code{object} (\code{default = "all"}). 
#' @param wCol    Vector, the columns of the scenario weights 
#'                of the \code{object} corresponding to different 
#'                stresses (\code{default = "all"}).
#' @param base    Logical, if \code{TRUE}, statistics under the baseline 
#'                are also returned (\code{default = "FALSE"}).
#'                
#' @return \code{summary.SWIM} returns a list with components
#'     corresponding to different stresses. Components contain a
#'     summary statistic of each column of the data of the 
#'     \code{SWIM} object:
#'     \tabular{ll}{
#'       \code{mean}        \tab The sample mean.\cr
#'       \code{sd}          \tab The sample standard deviation. \cr
#'       \code{skewness}    \tab The sample skewness.\cr
#'       \code{ex kurtosis} \tab The sample excess kurtosis\cr
#'       \code{1st Qu.}     \tab The 25\% quantile.\cr
#'       \code{Median}      \tab The median, 50\% quantile.\cr
#'       \code{3rd Qu.}     \tab The 75\% quantile.
#'     } 
#'     
#' @examples      
#' ## Example with the Relative Entropy
#' ## continuing example in stress_VaR 
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#'   
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = 0.9, q_ratio = 1.05)
#' summary(res1, xCol = "normal", base = TRUE) 
#' 
#' 
#' @author Silvana M. Pesenti 
#' 
#' @describeIn summary.SWIM Summarising Stressed Models
#' @seealso \code{\link{summary}}, \code{\link{SWIM}}
#' @export
#' 

summary.SWIM <- function(object, ..., xCol = "all", wCol = "all", base = FALSE){
  if (!is.SWIM(object)) stop("Wrong object")
  if (anyNA(object$x)) warning("x contains NA")

  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- get_weights(object)[ ,wCol]  

  summary_w <- apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .summary, x_data = x_data, cname = cname, base = base)
  names(summary_w) <- names(object$specs)[wCol]  
  
  if (base == TRUE){
    old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
    summary_base <- .summary(x_data = x_data, cname = cname, new_weights = old_weights)
    summary_w <- c(list("base" = summary_base), summary_w)
  }
    return(summary_w)
  }

  #' @describeIn summary.SWIM Summarising Stressed Models
  #' 
  #'     
  #' @examples      
  #' ## Example with the Wasserstein distance 
  #' \dontrun{
  #' resW <- stress_wass(type = "RM", x = x, 
  #' alpha = 0.9, q_ratio = 1.05)
  #' summary(resW, xCol = "normal", base = TRUE) 
  #' }
  #' 
  #' @author Zhuomin Mao
  #' 
  #' @export
  
  summary.SWIMw <- function(object, ..., xCol = "all", wCol = "all", base = FALSE){
    if (!is.SWIMw(object)) stop("Wrong object")
    if (anyNA(object$x)) warning("x contains NA")

    x_data <- as.matrix(get_data(object, xCol = xCol))
    cname <- colnames(x_data)
    if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
    new_weights <- get_weights(object)[ ,wCol]  
    
    summary_w <- apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .summary, x_data = x_data, cname = cname, base = base)
    names(summary_w) <- names(object$specs)[wCol]  
    
    if (base == TRUE){
      old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
      summary_base <- .summary(x_data = x_data, cname = cname, new_weights = old_weights)
      summary_w <- c(list("base" = summary_base), summary_w)
    }
    
    # Calculate the mean and standard deviation of SWIMw objects using integrals
    sd <- sd_stressed(object, xCol, wCol, base)
    mean <- mean_stressed(object, xCol, wCol, base)

    for (i in 1:length(summary_w)){
      summary_w[[i]][1, ] <- mean[i, ]
      summary_w[[i]][2, ] <- sd[i, ]
    }
    
    return(summary_w)
  }
  
  # help function, calculates summary when new_weights is a vector 
  .summary <- function(x_data, cname, new_weights, base){
    .temp <- function(y, w) apply(X = y, FUN = .moments, MARGIN = 2, w = w)
    moments_W <- .temp(x_data, new_weights)
    colnames(moments_W) <- cname
    rownames(moments_W) <- c("mean", "sd", "skewness", "ex kurtosis", "1st Qu.", "Median", "3rd Qu.")
    return(data.frame(moments_W))
  } 
  
  # help function calculates weighted moments, mean, sd, skewness, excess kurtosis, quartiles
  # x   numeric vector of observations 
  # w   numeric vector of weights
  
  .moments <- function(x, w){
    n <- length(as.vector(x))
    mean_w <- stats::weighted.mean(x = x, w = w)
    sd_w <- sqrt(mean(w * (x - mean_w)^2) * n / (n-1)) 
    skew_w <- mean(w * (x - mean_w)^3) / (sd_w^3) * n^2 / ((n-1) * (n-2))
    ex_kurt_w <- mean(w * (x - mean_w)^4) / (sd_w^4) - 3
    quartile_w <- as.matrix(Hmisc::wtd.quantile(x, weights = w, probs = c(0.25, 0.5, 0.75)))
    moments_w <- rbind(mean_w, sd_w, skew_w, ex_kurt_w, quartile_w)
    return(moments_w)
  }
  
  
  