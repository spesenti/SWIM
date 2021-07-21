#' Empirical Distribution Function of a Stressed Model
#' 
#' Provides the empirical distribution function of a stressed 
#'     model component (random variable) under the scenario weights. 
#' 
#' @inheritParams sensitivity 
#' @param xCol    Numeric or character, (name of) the columns of the underlying data 
#'                of the \code{object} (\code{default = 1}). 
#' @param wCol    Numeric, the columns of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' 
#' @return The empirical distribution function (a function) of 
#'     the \code{xCol} component of the stressed model with weights 
#'     \code{wCol}. The empirical distribution function can be 
#'     evaluated at a vector. 
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' grid <- seq(min(x$normal), max(x$normal), length.out = 5)
#' ## stressed empirical distribution function
#' cdf(res1, xCol = 1, wCol = 1)(grid)
#' ## baseline empirical distribution function
#' ecdf(x$normal)(grid)
#' 
#' @author Silvana M. Pesenti 
#' 
#' @seealso See \code{\link{plot_cdf}} for plotting the empirical 
#'     distribution function of the stressed model and 
#'     \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model. 
#' @export

cdf <- function(object, xCol = 1, wCol = 1){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class 'SWIM'")
  if (anyNA(object$x)) warning("x contains NA")
  if (length(wCol) > 1 || wCol == "all") stop("Input wCol has dimension larger than 1")
  if (length(xCol) > 1 || xCol == "all") stop("Input xCol has dimension larger than 1")
  
  
  w <- get_weights(object)[ , wCol]
  x_data <- get_data(object)[ , xCol]
  if (is.SWIM(object)){

    cdf <- .cdf(x = x_data, w = w)
    return(cdf)    
    
  } else {
    k <- object$specs$'stress 1'$k
    h <- object$h(x_data)
    if(is.character(k)) k_name <- k
    if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
    else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
    
    x_name <- colnames(get_data(object))[xCol]
    if(k_name == x_name){
      G.fn <- object$str.FY
    } else{
      G.fn <- function(x){
        return(sum(w * pnorm((x - x_data)/h)/length(x_data)))
      }
      G.fn <- Vectorize(G.fn)
    }
    return(G.fn)
  }
}

# tst 

# help function 
# x    numeric vector  
# w    numeric vector with weights
.cdf <- function(x, w){
  func <- function(t) {
    if (!is.vector(t)) stop("Given grid is not a vector")
    colMeans(w * (sapply(t, FUN = function(s) x <= s)))
    }
  return(func)
}
