#' Plotting Histograms of a Stressed Model
#' 
#' Plots the histogram of a stochastic model
#'     under the scenario weights.
#' 
#' @inheritParams  plot_cdf
#' 
#' @return If \code{displ = TRUE}, a histogram of the stochastic model 
#'     under the scenario weights.
#'     
#'     If \code{displ = FALSE}, a data.frame for customised plotting with 
#'     \code{ggplot}. The data.frame contains the columns: the column, 
#'     \code{xCol}, of the data of the stressed model, \code{stress} 
#'     (the stresses) and \code{value} (the values). \cr 
#'     Denote by \code{result} the return of the function call, then
#'     \code{ggplot} can be called via: 
#'     \deqn{ggplot(result, aes(x = result[ ,1], w = value, stat(density)))}
#'     \deqn{ + geom_freqpoly(binwidth = 0.2, aes(color = factor(stress))).}
#'      
#' @seealso See \code{\link{cdf}} and \code{\link{plot_cdf}} for 
#'     values and plotting of the empirical distribution 
#'     function of a stressed model, respectively, and 
#'     \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model.
#'      
#'@export

  plot_hist <- function(object, xCol = 1, wCol = "all", base = FALSE, x_limits, displ = TRUE){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (anyNA(object$x)) warning("x contains NA")
   x_data <- get.data(object)[ , xCol]
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(object))
  
   hist_data <- data.frame(x_data, get.weights(object)[ , wCol])
   names(hist_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "))
   if (base == TRUE){
    hist_data <- cbind(hist_data, rep(1, length(x_data)))
    names(hist_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "), "base")
   }
   hist_data <- reshape::melt(hist_data, id.var = paste("X", xCol, sep = ""), variable_name = "stress")

   if (displ == TRUE){
   if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
   ggplot2::ggplot(hist_data, ggplot2::aes(x = hist_data[,1], w = value, stat(density))) +
    ggplot2::geom_freqpoly(binwidth = 0.2, ggplot2::aes(color = factor(stress))) +
    ggplot2::labs(x = paste("X", xCol, sep = ""), y = "histogram") +
    ggplot2::xlim(x_limits) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
   return(hist_data)
   }
  }