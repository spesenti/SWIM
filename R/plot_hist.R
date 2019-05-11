# plots the histogram of one vector under different stresses

# x         SWIM object
# xCol      integer, colum of x
# wCol      integer, vector, colum of new_weights that are plotted 
# base      logical, if base = TRUE, original ecdf is plotted
# x_limits  xlim from the ggplot package
# disp      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 
# displ = FALSE: returns a data frame with the first colum the original data To plot a weighted histogram use weight option in ggplot: ggplot(df, aes(x = df, w = value, stat(density))). 

  plot_hist <- function(x, xCol = 1, wCol = "all", base = FALSE, x_limits, displ = TRUE){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   if (anyNA(x$x)) warning("x contains NA")
   x_data <- get.data(x)[ , xCol]
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get.weights(x))
  
   hist_data <- data.frame(x_data, get.weights(x)[ , wCol])
   names(hist_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "))
   if (base == TRUE){
    hist_data <- cbind(hist_data, rep(1, length(x_data)))
    names(hist_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "), "base")
   }
   require(reshape, quietly = TRUE)
   require(ggplot2, quietly = TRUE)
   hist_data <- melt(hist_data, id.var = paste("X", xCol, sep = ""), variable_name = "stress")

   if (displ == TRUE){
   if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
   ggplot(hist_data, aes(x = hist_data[,1], w = value, stat(density))) +
     geom_freqpoly(binwidth = 0.2, aes(color = factor(stress))) +
     labs(x = paste("X", xCol, sep = ""), y = "histogram") +
     xlim(x_limits) +
     theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
   } else {
   return(hist_data)
   }
  }