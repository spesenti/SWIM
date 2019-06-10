#' Sample Quantiles of a Stressed Model
#' 
#' Provides sample quantiles for components (random variables) of a 
#'     stochastic model, corresponding to distribution functions 
#'     under the scenario weights.
#'   
#' @details 
#' 
#' @inheritParams sensitivity 
#' @param probs   Vector of probabilities with values 
#'                in \code{[0,1]} (\code{default = (0, 0.25, 
#'                0.5, 0.75, 1)}).
#' @param wCol    Numeric, the column of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' @param type    Character, one of \code{"quantile","(i-1)/(n-1)",
#'                "i/(n+1)","i/n"}, with \code{default  = "quantile"}. 
#'                See details below. 
#' 
#' @details \code{type} defines the choice of algorithm used for 
#'     calculating the estimate of the sample quantiles.  
#'     \code{"quantile"} corresponds to the default interpolation used in  
#'     \code{\link[stats]{quantile}}. Further options are 
#'     \code{"(i-1)/(n-1)", "i/(n+1)", "i/n"} the inverse of the
#'     empirical distribution function, using, respectively, 
#'     \code{(wt - 1)/T, wt/(T+1), wt/T}, where \code{wt} is the 
#'     cumulative weight and \code{T} the total weight (usually total 
#'     sample size). See \code{\link[Hmisc]{wtd.quantile}} 
#'     for further details on \code{type}, on which
#'     \code{quantile_stressed} is based.
#'     
#' @return Returns a matrix with estimates of the distribution quantiles
#'     at the probabilities, \code{probs}, under the scenario weights 
#'     \code{wCol}. 
#' 
#'                 
#' @seealso See \code{\link[Hmisc]{wtd.quantile}} on which the function
#'     \code{quantile_stressed} is based. \cr
#'     See \code{cdf} for the empirical distribution function of 
#'     a stressed model.
#'     
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed sample quantiles  
#' quantile_stressed(res1, probs = seq(0.9, 0.99, 0.01), wCol = 2)    
#'     
#' @export

  quantile_stressed <- function(object, probs = seq(0, 1, 0.25), xCol = "all", 
                                wCol = 1, type = c("quantile","(i-1)/(n-1)",
                                "i/(n+1)","i/n")){
   if (!is.SWIM(object)) stop("Wrong object")
   if (missing(type)) type <- as.character("quantile")
   if (anyNA(object$x)) warning("x contains NA")
   new_weights <- get.weights(object)[ , wCol]
   if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get.data(object))
   if (is.null(colnames(get.data(object)))){
    cname <-  paste("X", as.character(xCol), sep = "")
   } else if (!is.character(xCol)){
    cname <- colnames(get.data(object))[xCol]
   } else {
    cname <- xCol   
   }
   x_data <- as.matrix(get.data(object)[ , xCol])

   quantile_w <- as.matrix(apply(X = as.matrix(x_data), MARGIN = 2, FUN = Hmisc::wtd.quantile, weights = new_weights, probs = probs, type = type))
   if (length(probs) == 1 && length(cname) > 1) quantile_w <- matrix(quantile_w, nrow = 1)
   colnames(quantile_w) <- cname
   rownames(quantile_w) <- paste(probs * 100, "%", sep = "")
   return(quantile_w)
  }