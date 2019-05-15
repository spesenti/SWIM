#' Importance Ranking for a Stressed Model
#' 
#' Provides the importance ranks of random variables  
#'     of a stressed model for different sensitivity measures. 
#'     
#' @inheritParams sensitivity
#' @param type    Character, one of \code{"Gamma", "Wasserstein", "all"}.
#' 
#' @details For the definition of the sensitivity 
#'     measures (\code{type}), see \code{\link{sensitivity}}.
#'     
#' @return A data.frame containing the importance ranks of the 
#'     stressed model for different sensitivity measures. Rows correspond 
#'     to different random variables. The last two rows specify the 
#'     \code{stress} and \code{type} of the sensitivity measure on 
#'     which the ranking is calculated.
#' 
#' @seealso See \code{\link{sensitivity}} for the values of the 
#'     sensitivity measures, \code{\link{plot_sensitivity}} for plotting 
#'     sensitivity measures and \code{\link{summary}} for a 
#'     summary statistic of a stressed model.     
#' 
#' @export

  importance_rank <- function(object, xCol = "all", wCol = "all", 
    type = c("Gamma", "Wasserstein", "all"), f = NULL){
   if (!is.SWIM(object)) stop("Wrong object")
   if (anyNA(object$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
  
   sens_w <- sensitivity(object, xCol = xCol, wCol = wCol, type = type, f = f)
   if (length(sens_w) < 4) stop("Only one input provided.")
   rank_w <- t(apply(X = sens_w[ , 1:(length(sens_w) - 2)], MARGIN = 1, FUN = function(z) rank(z, ties.method = "min")))
   rank_w <- cbind(rank_w, sens_w[ , (length(sens_w) - 1):length(sens_w)])
    
   if (is.character(type) && type == "all") rank_w <- rank_w[-which(rank_w[,"type"] == "Kolmogorov"), ]
   return(rank_w)
  }