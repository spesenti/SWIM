# This function plots the rank
# Input: 
# x         object
# xCol      integer vector, columns of x$x (default = "all")  
# wCol      integer vector, columns of new_weights, (default = "all")
# type      character vector, c("Gamma", "Wasserstein", "all"). The Kolmogorov distance is the same for all inputs. (default = "Gamma")
# f         list of functions, calcualted the sensitivity of the transformed input vector. List needs to have the same length as xCol. 
# displ      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 


plot_sensitivity <- function(x, xCol = "all", wCol = "all", type = c("Gamma", "Wasserstein", "all"), f = NULL, displ = TRUE){
  
  if(!is.SWIM(x)) stop("wrong object")
  if(anyNA(x$x)) warning("'x' contains NA")
  if(missing(type)) type <- "all"
  require(ggplot2, quietly = TRUE)
  require(reshape, quietly = TRUE)
  
  sens <- sensitivity(x, xCol = xCol, wCol = wCol, type = type, f)
  sens <- melt(sens, id.var = c("stress", "type"), variable_name = "X_all")
  if(displ == TRUE){
    if(type == "all" | length(levels(as.factor(sens$stress))) > 1){
  ggplot(sens, aes(x = X_all, y = value)) +
      geom_jitter(height = 0, width = 0.15, aes(color = factor(stress), shape = type)) +
      labs(x = "", y = "sensitivites") +
   theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
    }else{
      ggplot(sens, aes(x = X_all, y = value)) +
        geom_point(aes(color = factor(stress), shape = type)) +
        labs(x = "", y = "sensitivites") +
        theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
    }
  }else{
    return(sens)
  }
} 




