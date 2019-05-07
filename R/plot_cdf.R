# plots the empirical distribution function of one vector under different stresses

# x         SWIM object
# xCol      integer, colum of x
# wCol      integer, vector, colum of new_weights that are plotted 
# base      logical, if base = TRUE, original ecdf is plotted
# n         integer, number of points used to plot of stat_ecdf in ggplot (default = 500)
# disp      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 
# displ = TRUE: returns a data frame with the first colum the original data To plot a weighted histogram use weight option in ggplot: ggplot(df, aes(x = df, w = value, stat(density))). 

plot_cdf <- function(x, xCol = 1, wCol = "all", base = FALSE, n = 500, x_limits, displ = TRUE){
    if(!is.SWIM(x)) stop("object is not of class 'SWIM'")
    if(anyNA(x$x)) warning("'x' contains NA")
    
    x_data <- get.data(x)[, xCol]
    if(is.character(wCol) && wCol == "all") wCol <- 1:dim(get.weights(x))[2]
    
    plot_data <- data.frame(x_data, get.weights(x)[, wCol])
    names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "))
    if(base == TRUE){
      plot_data <- cbind(plot_data, rep(1, length(x_data)))
      names(plot_data) <- c(paste("X", xCol, sep = ""), paste("stress", wCol, sep = " "), "base")
    }
    require(reshape, quietly = TRUE)
    require(ggplot2, quietly = TRUE)
    # the value.name = "new_weights"
    plot_data <- melt(plot_data, id.var = paste("X", xCol, sep = ""), variable_name = "stress")
    
    if(displ == TRUE){
      if(missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)) 
      ggplot(plot_data, aes(x = plot_data[,1], w = value)) +
        stat_ecdf(aes(color = factor(stress)), n = n) +
        labs(x = paste("X", xCol, sep = ""), y = "ecdf") +
        xlim(x_limits) +
        theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
    }else{
      return(plot_data)
    }
  }



