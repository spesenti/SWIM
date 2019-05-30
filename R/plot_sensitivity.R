#' Plotting Sensitivities of a Stressed Model
#'
#' Plots the sensitivity measures for components (random variables) 
#'     of a stochastic model under the scenario weights.
#'  
#' @inheritParams sensitivity
#' @param type    Character, one of \code{"Gamma", "Wasserstein"} 
#'                (\code{default = "Gamma"}).
#' @param displ   Logical, if \code{TRUE} the plot is displayed, 
#'                otherwise the data.frame for customised plotting with 
#'                \code{ggplot} is returned (\code{default = TRUE}). 
#'                
#' @details For the definition of the sensitivity 
#'     measures (\code{type}), see \code{\link{sensitivity}}.
#'                
#' @return If \code{displ = TRUE}, a plot displaying the sensitivity
#'     measures of the stochastic model under the scenario weights.
#'     
#'     If \code{displ = FALSE}, a data.frame for customised plotting with 
#'     \code{ggplot}. The data.frame
#'     contains the columns: \code{stress} (the stresses), \code{type} 
#'     (the types of sensitivity), \code{X_all} (the random variables),
#'     \code{value} (the values of the sensitivities). \cr 
#'     Denote by \code{result} the return of the function call, then
#'     \code{ggplot} can be called via: 
#'     \deqn{ggplot(result, aes(x = X_all, y = value))}
#'     \deqn{ + geom_point(aes(color = factor(stress), shape = type)).}
#'      
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' 
#' plot_sensitivity(res1, wCol = 1:2, type = "Gamma")     
#' plot_sensitivity(res1, xCol = 1:2, type = "Wasserstein")     
#'      
#' @seealso See \code{\link{sensitivity}} for the values of the 
#'     sensitivity measures of a stressed model and    
#'     \code{\link{importance_rank}} for ranking of random
#'     variables according to their sensitivities.  
#'     
#' @export

  plot_sensitivity <- function(object, xCol = "all", wCol = "all", type = c("Gamma", "Wasserstein"), f = NULL, displ = TRUE){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (anyNA(object$x)) warning("x contains NA")
   sens <- sensitivity(object, xCol = xCol, wCol = wCol, type = type, f)
   sens <- reshape2::melt(sens, id.var = c("stress", "type"), variable.name = "X_all")
   if (displ == TRUE){
     ggplot2::ggplot(sens, ggplot2::aes_(x = ~X_all, y = ~value)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(stress), shape = type)) +
      ggplot2::labs(x = "", y = "sensitivity") +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
    return(sens)
   }
  }