#' plots empirical distribution
#' @export

# plots the empirical distribution function of one vector under different stresses

# x         SWIM object
# xCol      integer, colum of x
# wCol      integer, vector, colum of new_weights that are plotted 
# base      logical, if base = TRUE, original ecdf is plotted
# n         integer, number of points used to plot of stat_ecdf in ggplot (default = 500)
# disp      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 
# displ = TRUE: returns a data frame with the first colum the original data To plot a weighted histogram use weight option in ggplot: ggplot(df, aes(x = df, w = value, stat(density))). 

  plot_cdf <- function(x, xCol = 1, wCol = "all", base = FALSE, n = 500, x_limits, displ = TRUE){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   if (anyNA(x$x)) warning("x contains NA")
   x_data <- get.data(x)[, xCol]
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(x))
    
   plot_data <- data.frame(x_data, get.weights(x)[ , wCol])
   names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "))
   if (base == TRUE){
    plot_data <- cbind(plot_data, rep(1, length(x_data)))
    names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "), "base")
   }
   plot_data <- reshape::melt(plot_data, id.var = paste("X", xCol, sep = ""), variable_name = "stress")
    
   if (displ == TRUE){
    if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
    ggplot2::ggplot(plot_data, aes(x = plot_data[,1], w = value)) +
      ggplot2::stat_ecdf(aes(color = factor(stress)), n = n) +
      ggplot2::labs(x = paste("X", xCol, sep = ""), y = "ecdf") +
      ggplot2::xlim(x_limits) +
      ggplot2::theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
   } else {
    return(plot_data)
   }
  }