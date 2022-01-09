#' Plotting the scenario weights of a Stressed Model
#'
#' Plots the scenario weights of a stressed model against a model component.
#'
#' @inheritParams  plot_cdf
#' @inheritParams  sensitivity
#' @param n        Integer, the number of points used to plot 
#'                 (\code{default = 5000} or the minimum of the data). If 
#'                 \code{n = "all"}, all data points are plotted. If \code{n} is
#'                 a subset of points, the plotted scenario weights are chosen in 
#'                 an equidistant way.  
#'                 
#
#' @return If \code{displ = TRUE}, a plot displaying the scenario
#'     weights of a stochastic model against a model component.
#'
#'     If \code{displ = FALSE}, a data.frame for customised plotting with
#'     \code{ggplot}. The data.frame contains the following columns:
#'     \code{grid}, the grid points to plot the quantiles,
#'     \code{stress} (the stresses) and \code{value} (the quantile values). \cr
#'     Denote by \code{res} the return of the function call, then
#'     \code{ggplot} can be called via:
#'     \deqn{ggplot(res, aes(x = res[ ,1], y = value))}
#'     \deqn{ + geom_lines(aes(color = factor(stress))).}
#'
#' @examples
#'  \donttest{
#' ## example with a stress with \code{credit_data} data set:
#' data("credit_data")
#' ## two stresses in VaR
#' model_stress <- stress_VaR(credit_data, alpha = c(0.9, 0.95), q_ratio = 1.1, k =1) 
#' plot_weights(model_stress, xCol = "L", wCol = 1:2)
#' 
#' ## additional stress on VaR and ES
#' model_stress <- stress_VaR_ES(model_stress, alpha = 0.9, q_ratio = 1.1, s_ratio = 1.2, k =1) 
#' plot_weights(model_stress, xCol = "L", wCol = "all", n = 1000, x_limits = c(0, 3500), 
#'              y_limits = c(0, 10))
#'              }
#'
#' @seealso See \code{\link{plot_quantile}} for plotting sample quantiles of a 
#'     stressed model and \code{\link{plot_cdf}} for plotting empirical 
#'     distribution functions.
#'
#' @export
#' 
plot_weights <- function(object, xCol = 1, wCol = "all", n, x_limits, y_limits, displ = TRUE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
  if (anyNA(object$x)) warning("x contains NA")
  if(is.numeric(xCol) && (length(xCol) != 1)) stop("Invalid xCol argument.")
  if(is.character(xCol) && (!(xCol %in% colnames(get_data(object))))) stop("Invalid xCol argument.")
  x_data <- get_data(object)[, xCol]
  if(is.character(xCol)) x_name <- xCol
  if(is.null(colnames(get_data(object)))) x_name <- paste("X", xCol, sep = "") else if(!is.character(xCol)) x_name <- colnames(get_data(object))[xCol]
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  plot_data <- data.frame(x_data, get_weights(object)[ , wCol])
  
  # Display components' names
  names(plot_data) <- c(x_name, names(object$specs)[wCol])
  
  if(missing(n)){
    n <- min(5000, dim(get_data(object))[1])
  } else {
    if(!is.numeric(n) && (n != "all")) stop("n not a integer or 'all'.")
    if(!is.null(n) & is.numeric(n)){
      grid <- seq(1, dim(get_data(object))[1], length.out = n)
      plot_data <- plot_data[order(plot_data[, 1]),][grid,]
    }
    if(n == "all"){
      grid <- 1:length(x_data)
    }
  }
    
  plot_data <- reshape2::melt(plot_data, id.var = x_name, variable.name = "stress", value.name = "value")
  
  if (displ == TRUE){
    if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data))
    if (missing(y_limits)) y_limits <- c(min(plot_data[, "value"]), max(plot_data[, "value"]))
    ggplot2::ggplot(plot_data, ggplot2::aes_(x = plot_data[, 1], y = ~value)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(stress))) +
      ggplot2::labs(x = x_name , y = "scenario weights") +
      ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
  } else {
    return(plot_data)
  }
}
