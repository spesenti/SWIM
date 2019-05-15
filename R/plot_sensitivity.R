#' plots sensitivity
#' @export
#  This function plots the rank
# Input: 
# object         object
# xCol      integer vector, columns of x$x (default = "all")  
# wCol      integer vector, columns of new_weights, (default = "all")
# type      character vector, c("Gamma", "Wasserstein", "all"). The Kolmogorov distance is the same for all inputs. (default = "Gamma")
# f         list of functions, calcualted the sensitivity of the transformed input vector. List needs to have the same length as xCol. 
# displ      logical, If TRUE, the plot is displayed, if FALSE the data for plotting is returned. 


  plot_sensitivity <- function(object, xCol = "all", wCol = "all", type = c("Gamma", "Wasserstein", "all"), f = NULL, displ = TRUE){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (anyNA(object$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
   sens <- sensitivity(object, xCol = xCol, wCol = wCol, type = type, f)
   sens <- reshape::melt(sens, id.var = c("stress", "type"), variable_name = "X_all")
   if (displ == TRUE){
     ggplot2::ggplot(sens, ggplot2::aes(x = X_all, y = value)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(stress), shape = type)) +
      ggplot2::labs(x = "", y = "sensitivites") +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
    return(sens)
   }
  }