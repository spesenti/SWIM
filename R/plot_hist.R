#' Plotting Histograms of a Stressed Model
#' 
#' Plots the histogram of a stochastic model
#'     under the scenario weights.
#' 
#' @inheritParams  plot_cdf
#' @param binwidth Numeric, the width of the bins used to plot 
#'                 the histogram, the \code{binwidth} in the 
#'                 \code{geom_histogram} function  in \code{ggplot} 
#'                 (default corresponds to 40 bins).   
#' 
#' @return If \code{displ = TRUE}, a histogram of the stochastic model 
#'     under the scenario weights.
#'     
#'     If \code{displ = FALSE}, a data.frame for customised plotting with 
#'     \code{ggplot}. The data.frame contains the columns: the column, 
#'     \code{xCol}, of the data of the stressed model, \code{stress} 
#'     (the stresses) and \code{value} (the values). \cr 
#'     Denote by \code{res} the return of the function call, then
#'     \code{ggplot} can be called via: 
#'     \deqn{ggplot(res, aes(x = res[ ,1]))}
#'     \deqn{ + geom_histogram(weight = value, aes(color = factor(stress))).}
#'  
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(10^5), 
#'   "gamma" = rgamma(10^5, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' plot_hist(res1, xCol = 1, wCol = 1:2, base = TRUE, binwidth = 0.1)
#'                  
#' @seealso See \code{\link{cdf}} and \code{\link{plot_cdf}} for 
#'     values and plotting of the empirical distribution 
#'     function of a stressed model, respectively, and 
#'     \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model.
#'      
#'@export

  plot_hist <- function(object, xCol = 1, wCol = "all", base = FALSE, x_limits, displ = TRUE, binwidth){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (anyNA(object$x)) warning("x contains NA")
   x_data <- get.data(object)[ , xCol]
   if(missing(binwidth)) binwidth <- (max(x_data) - min(x_data)) / 40
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(object))
  
   hist_data <- data.frame(x_data, get.weights(object)[ , wCol])
   if(is.character(xCol)) x_name <- xCol
   if(is.null(colnames(get.data(object)))) x_name <- paste("X", xCol, sep = "") else if(!is.character(xCol)) x_name <- colnames(get.data(object))[xCol]
   names(hist_data) <- c(x_name, paste("stress", wCol, sep = " "))
   if (base == TRUE){
    hist_data <- cbind(hist_data, "base" = rep(1, length(x_data)))
   }
   hist_data <- reshape2::melt(hist_data, id.var = x_name, variable.name = "stress", value.name = "value")

   if (displ == TRUE){
   if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
   ggplot2::ggplot(hist_data, ggplot2::aes(x = hist_data[,1])) +
   ggplot2::geom_histogram(binwidth = binwidth, ggplot2::aes(color = factor(stress), weight = value, fill = factor(stress))) +
    ggplot2::labs(x = x_name, y = "histogram") +
    ggplot2::xlim(x_limits) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
   return(hist_data)
   }
  }