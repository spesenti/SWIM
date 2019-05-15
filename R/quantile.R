#' Sample Quantiles of a Stressed Model
#' 
#' Provides the sample quantiles of a stressed model. 
#'   
#' @details 
#' 
#' @inheritParams sensitivity 
#' @param probs   Numeric vector of probabilities with values 
#'                in \code{[0,1]} (\code{default = (0, 0.25, 
#'                0.5, 0.75, 1)}).
#' @param wCol    Numeric, the column of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' @param type    Character, one of \code{"quantile","(i-1)/(n-1)",
#'                "i/(n+1)","i/n"}, see details below.
#' 
#' @details The \code{type} corresponds to the algorithm for calcualting
#'     the estimate of the sample quantile. \code{"quantile"} 
#'     corresponds to the same interpolation as 
#'     \code{\link[stats]{quantile}}. 
#'     \code{"(i-1)/(n-1)", "i/(n+1)", "i/n"} are the inverse of the empirical distribution function, using, respectively, \code{(wt - 1)/T, wt/(T+1), wt/T}, where \code{wt} is the cumulative weight and \code{T} is the total weight (usually total sample size). See 
#'     \code{\link[Hmisc]{wtd.quantile}} on which the function
#'     \code{quantile_stressed} is based.
#'                                
#' @return Returns estimates of distribution quantiles for 
#'     scenario weight \code{wCol} at the probabilities in \code{probs}. 
#' 
#'                 
#' @seealso See \code{\link[Hmisc]{wtd.quantile}} on which the function
#'     \code{quantile_stressed} is based. \cr
#'     See \code{cdf} for the empifical distribution function of 
#'     a stressed model.
#'     
#' @export

  quantile_stressed <- function(object, probs = seq(0, 1, 0.25), xCol = "all", wCol = 1, type = c("quantile","(i-1)/(n-1)","i/(n+1)","i/n")){
   if (!is.SWIM(object)) stop("Wrong object")
   if (missing(type)) type <- as.character("quantile")
   if (anyNA(object$x)) warning("x contains NA")
   new_weights <- get.weights(object)[ , wCol]
   if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get.data(object))
   if (is.null(colnames(get.data(object)))){
    cname <-  paste("X", as.character(xCol), sep = "")
   } else {
    cname <- colnames(get.data(object))[xCol]
   } 
   x_data <- as.matrix(get.data(object)[ , xCol])

   quantile_w <- as.matrix(apply(X = as.matrix(x_data), MARGIN = 2, FUN = Hmisc::wtd.quantile, weights = new_weights, probs = probs, type = type))
   if (length(probs) == 1 && length(cname) > 1) quantile_w <- matrix(quantile_w, nrow = 1)
   colnames(quantile_w) <- cname
   rownames(quantile_w) <- paste(probs * 100, "%", sep = "")
   return(quantile_w)
  }