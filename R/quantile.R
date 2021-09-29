#' Sample Quantiles of a Stressed Model
#' 
#' Provides sample quantiles for components (random variables) of a 
#'     stochastic model, corresponding to distribution functions 
#'     under the scenario weights.
#'   
#' @details 
#' 
#' @inheritParams summary.SWIM 
#' @param probs   Vector of probabilities with values 
#'                in \code{[0,1]} (\code{default = (0, 0.25, 
#'                0.5, 0.75, 1)}).
#' @param wCol    Numeric, the column of the scenario weights 
#'                of the \code{object} (\code{default = 1}).
#' @param type    Character, one of \code{"quantile","(i-1)/(n-1)",
#'                "i/(n+1)","i/n"}, (\code{default  = "quantile"}). 
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
#'     \code{quantile_stressed} is based. \code{type} is ignored for 
#'     when evaluating quantiles for \code{SWIMw} objects.
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
#' @author Silvana M. Pesenti, Zhuomin Mao
#'     
#' @export

quantile_stressed <- function(object, probs = seq(0, 1, 0.25), xCol = "all", 
                             wCol = 1, type = c("quantile","(i-1)/(n-1)",
                             "i/(n+1)","i/n"), base = FALSE){
if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
if (missing(type)) type <- as.character("quantile")
if (anyNA(object$x)) warning("x contains NA")
if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
if (is.null(colnames(get_data(object)))){
 cname <-  paste("X", as.character(xCol), sep = "")
} else if (!is.character(xCol)){
 cname <- colnames(get_data(object))[xCol]
} else {
 cname <- xCol   
}
if (length(wCol) > 1 || wCol == "all") stop("Input wCol has dimension larger than 1")

new_weights <- get_weights(object)[ , wCol]
x_data <- as.matrix(get_data(object)[ , xCol])

if (is.SWIM(object)){
   # K-L Divergence
   quantile_w <- as.matrix(apply(X = as.matrix(x_data), MARGIN = 2, FUN = Hmisc::wtd.quantile, weights = new_weights, probs = probs, type = type))
   if (length(probs) == 1 && length(cname) > 1) quantile_w <- matrix(quantile_w, nrow = 1)
   colnames(quantile_w) <- cname
   rownames(quantile_w) <- paste(probs * 100, "%", sep = "")
   
   if (base == TRUE){
      old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
      quantile_base <- as.matrix(apply(X = as.matrix(x_data), MARGIN = 2, FUN = Hmisc::wtd.quantile, weights = old_weights, probs = probs, type = type))
      colnames(quantile_base) <- paste("base", cname, sep=' ')
      quantile_w <- cbind(quantile_w, quantile_base)
   }
   
   return(quantile_w)
} else {
   # Wasserstein Distance
   w <- get_weights(object)[ , wCol]
   x_data <- get_data(object)
   h <- object$h[[wCol]](x_data)
   
   index <- names(object$specs)[wCol]
   k <- object$specs[[index]]$k
   
   if(is.character(k)) k_name <- k
   if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
   else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
   
   quantile_w <- c()
   col_names <- c()
   
   for (c in cname){
      lower_bracket = min(x_data[, c])
      upper_bracket = max(x_data[, c])
      
      
      if(k_name == c){
         # Get stressed quantile function
         G.inv.fn <- Vectorize(object$str_FY_inv[[wCol]])
      } else{
         # Get KDE
         G.fn <- function(x){
            return(sum(w * stats::pnorm((x - x_data[,c])/h)/length(x_data[,c])))
         }
         G.fn <- Vectorize(G.fn)
         G.inv.fn <- Vectorize(.inverse(G.fn, lower_bracket, upper_bracket))
      }
      quantile_w <- cbind(quantile_w, G.inv.fn(probs))
      col_names <- cbind(col_names, c)
      
      if (base == TRUE){
         # Get base KDEs
         F.fn <- function(x){
            return(sum(stats::pnorm((x - x_data[, c])/h)/length(x_data[, c])))
         }
         F.fn <- Vectorize(F.fn)
         F.inv.fn <- Vectorize(.inverse(F.fn, lower_bracket, upper_bracket))
         quantile_w <- cbind(quantile_w, F.inv.fn(probs))
         col_names <- cbind(col_names, paste("base", c, sep=" "))
      }
   }
   
   colnames(quantile_w) <- col_names
   rownames(quantile_w) <- paste(probs*100, "%", sep="")
   
   return(quantile_w)
}

}

# helper
.inverse <- function(f, lower = -100, upper = 100){
  return(function(y){stats::uniroot((function(x){f(x) - y}), lower = lower, upper = upper, extendInt = 'yes')$root})
}
