#' Plotting the Empirical Distribution Functions of a Stressed Model
#' 
#' Plots the empirical distribution function of a stochastic model
#'     under the scenario weights.
#'  
#' @inheritParams  sensitivity
#' @inheritParams  plot_sensitivity
#' @inheritParams  summary.SWIM
#' @param xCol     Numeric, the column of the underlying data 
#'                 of the \code{object} (\code{default = 1}). 
#' @param n        Integer, the number of points used to plot 
#'                 \code{stat_ecdf} in \code{ggplot} (\code{default 
#'                 = 500}).
#' @param x_limits Vector, the limits of the x-axis of the plot, the 
#'                 value for the \code{xlim} function in \code{ggplot}.
#                 
#' @return If \code{displ = TRUE}, a plot displaying the empirical
#'     distribution function of the stochastic model under the 
#'     scenario weights.
#'     
#'     If \code{displ = FALSE}, a data.frame for customised plotting with 
#'     \code{ggplot}. The data.frame contains the columns: the column, 
#'     \code{xCol}, of the data of the stressed model, 
#'     \code{stress} (the stresses) and \code{value} (the values). \cr 
#'     Denote by \code{result} the return of the function call, then
#'     \code{ggplot} can be called via: 
#'     \deqn{ggplot(result, aes(x = result[ ,1], w = value))}
#'     \deqn{ + stat_ecdf(aes(color = factor(stress)), n = n).}
#'      
#' @seealso See \code{\link{cdf}} for the empirical distribution function 
#'     of a stressed model and \code{\link{quantile_stressed}} for
#'     sample quantiles of a stressed model.
#'     
#' @export

  plot_cdf <- function(object, xCol = 1, wCol = "all", base = FALSE, n = 500, x_limits, displ = TRUE){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (anyNA(object$x)) warning("x contains NA")
   x_data <- get.data(object)[, xCol]
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(object))
    
   plot_data <- data.frame(x_data, get.weights(object)[ , wCol])
   names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "))
   if (base == TRUE){
    plot_data <- cbind(plot_data, rep(1, length(x_data)))
    names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "), "base")
   }
   plot_data <- reshape::melt(plot_data, id.var = paste("X", xCol, sep = ""), variable_name = "stress")
    
   if (displ == TRUE){
    if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
    ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_data[,1], w = value)) +
      ggplot2::stat_ecdf(ggplot2::aes(color = factor(stress)), n = n) +
      ggplot2::labs(x = paste("X", xCol, sep = ""), y = "ecdf") +
      ggplot2::xlim(x_limits) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
    return(plot_data)
   }
  }