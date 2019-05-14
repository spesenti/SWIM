#' plots sensitivity
#' @export
#  This function plots the rank
# Input: 
# x         object
# xCol      integer vector, columns of x$x (default = "all")  
# wCol      integer vector, columns of new_weights, (default = "all")
# type      character vector, c("Gamma", "Wasserstein", "all"). The Kolmogorov distance is the same for all inputs. (default = "Gamma")
# f         list of functions, calcualted the sensitivity of the transformed input vector. List needs to have the same length as xCol. 
# displ      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 


  plot_sensitivity <- function(x, xCol = "all", wCol = "all", type = c("Gamma", "Wasserstein", "all"), f = NULL, displ = TRUE){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   if (anyNA(x$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
   sens <- sensitivity(x, xCol = xCol, wCol = wCol, type = type, f)
   sens <- reshape::melt(sens, id.var = c("stress", "type"), variable_name = "X_all")
   if (displ == TRUE){
     ggplot2::ggplot(sens, ggplot2::aes(x = X_all, y = value)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(stress), shape = type)) +
      ggplot2::labs(x = "", y = "sensitivites") +
      ggplot2::theme(legend.title = element_blank(), legend.key = element_blank(), legend.text = element_text(size = 10))
   } else {
    return(sens)
   }
  }