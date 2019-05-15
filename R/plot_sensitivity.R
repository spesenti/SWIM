#' Plotting Sensitivities of a Stressed Model
#'
#' Plotting of the sensitivity measures of a stressed model.
#'  
#' @inheritParams sensitivity
#' @param type    Character, one of \code{"Gamma", "Wasserstein", "all"}.
#' @param displ   Logical, if \code{TRUE} the plot is displayed, 
#'                otherwise the data.frame for customised plotting with 
#'                \code{ggplot} is returned (\code{default = TRUE}). 
#'                
#' @details For the definition of the sensitivity 
#'     measures (\code{type}), see \code{\link{sensitivity}}.
#'                
#' @return If \code{displ = TRUE}, a plot displaying the sensitivity
#'     measures of the stressed model. 
#'     
#'     If \code{displ = FALSE}, a data.frame containing the values 
#'     of the sensitivity measures of the stressed model. The data.frame
#'     contains the columns: \code{stress} (the stresses), \code{type} (the types of sensitivity), \code{X_all} (the random variables), \code{value} (the values of the sensitivities). \cr 
#'     Denote by \code{result} the return of the function call, then \code{ggplot} can be called via: 
#'     \deqn{ggplot(result, aes(x = X_all, y = value))}
#'     \deqn{ + geom_point(aes(color = factor(stress), shape = type)).}
#'      
#' @seealso See \code{\link{sensitivity}} for the values of the 
#'     sensitivity measures of a stressed model and    
#'     \code{\link{importance_rank}} for ranking of random
#'     variables according to their sensitivities.  
#'     
#' @export

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