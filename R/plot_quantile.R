#' Plotting Quantile Functions of a Stressed Model
#'
#' Plots the empirical quantile function of a stressed SWIM model
#'     component (random variable) or KDE quantile function of a stressed
#'     SWIMw model component under the scenario weights.
#'
#' @inheritParams  plot_cdf
#' @inheritParams  sensitivity
#' @param n        Integer, the number of points used to plot 
#'                 (\code{default = 500}).
#
#' @return If \code{displ = TRUE}, a plot displaying the empirical or KDE 
#'     quantile function of the stochastic model under the
#'     scenario weights.
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
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(10 ^ 5),
#'   "gamma" = rgamma(10 ^ 5, shape = 2)))
#' res1 <- stress(type = "VaR", x = x,
#'   alpha = c(0.75, 0.95), q_ratio = 1.15)
#' plot_quantile(res1, xCol = 1, wCol = 1:2, base = TRUE)
#' plot_quantile(res1, xCol = 1, wCol = 1:2, base = TRUE, x_limits = c(0.8, 1), 
#'               y_limits = c(0, 5))
#'
#' @author Silvana M. Pesenti, Zhuomin Mao
#'
#' @seealso See \code{\link{quantile_stressed}} for sample quantiles of a 
#'     stressed model and \code{\link{plot_cdf}} for plotting empirical or KDE 
#'     distribution functions under scenario weights.
#'
#' @export
#' 
plot_quantile <- function(object, xCol = 1, wCol = "all", base = FALSE, n = 500, x_limits, y_limits, displ = TRUE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
  if (anyNA(object$x)) warning("x contains NA")
  if(is.numeric(xCol) && (length(xCol) != 1)) stop("Invalid xCol argument.")
  if(is.character(xCol) && (!(xCol %in% colnames(get_data(object))))) stop("Invalid xCol argument.")

  if (is.character(xCol)) x_name <- xCol
  if (is.null(colnames(get_data(object)))) x_name <- paste("X", xCol, sep = "") else if(!is.character(xCol)) x_name <- colnames(get_data(object))[xCol]
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
    
  if (is.SWIM(object)){
    # K-L Divergence
    grid <- seq(0, 1, length.out = n)
    quant_data <- cbind(grid, sapply(wCol, FUN = quantile_stressed, object = object, probs = grid, xCol = xCol,type = c("quantile")))
    colnames(quant_data) <-  c("grid", names(object$specs)[wCol])
    if (base == TRUE){
      quant_data <- cbind(quant_data, base = as.numeric(stats::quantile(get_data(object)[, xCol], grid)))
    }
    
    plot_data <- reshape2::melt(as.data.frame(quant_data), id.var = "grid", variable.name = "stress", value.name = "value")
    
    if (displ == TRUE){
      if (missing(x_limits)) x_limits <- c(0,1)
      if (missing(y_limits)) y_limits <- c(min(get_data(object)[, xCol]), max(get_data(object)[, xCol]))
      ggplot2::ggplot(plot_data, ggplot2::aes_(x = plot_data[,1], y = ~value)) +
        ggplot2::geom_line(ggplot2::aes(color = factor(stress)), n = n) +
        ggplot2::labs(x = "" , y = paste("quantiles of", x_name, sep = " ")) +
        ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
    } else {
      return(plot_data)
    }
  } else {
    # Wasserstein Distance
    grid <- object$u
    quant_data <- data.frame(grid)
    for (i in 1:length(wCol)) {
      w <- get_weights(object)[ , i]
      x_data <- get_data(object)[, i]
      h <- object$h[[i]](x_data)
      
      index <- names(object$specs)[i]
      k <- object$specs[[index]]$k
      if(is.character(k)) k_name <- k
      if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
      else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
      
      lower_bracket = min(x_data)-(max(x_data)-min(x_data))*0.1
      upper_bracket = max(x_data)+(max(x_data)-min(x_data))*0.1
      
      if(k_name == x_name){
        # Get stressed distribution
        G.inv.fn <- Vectorize(object$str_FY_inv[[i]])
      } else{
        # Get KDE
        G.fn <- function(x){
          return(sum(w * stats::pnorm((x - x_data)/h)/length(x_data)))
        }
        G.fn <- Vectorize(G.fn)
        G.inv.fn <- Vectorize(.inverse(G.fn, lower_bracket, upper_bracket))
      }
      
      if (is.SWIMw(object)){
        quant_data <- cbind(quant_data, G.inv.fn(grid))
      }
    }
    colnames(quant_data) <-  c("grid", names(object$specs)[wCol])
    
    if (base == TRUE){
      # Get KDE
      F.fn <- function(x){
        return(sum(stats::pnorm((x - x_data)/h)/length(x_data)))
      }
      F.fn <- Vectorize(F.fn)
      F.inv.fn <- Vectorize(.inverse(F.fn, lower_bracket, upper_bracket))
      quant_data <- cbind(quant_data, base = F.inv.fn(grid))
    }
    
    plot_data <- reshape2::melt(as.data.frame(quant_data), id.var = "grid", variable.name = "stress", value.name = "value")
    
    if (displ == TRUE){
      if (missing(x_limits)) x_limits <- c(0,1)
      if (missing(y_limits)) y_limits <- c(min(get_data(object)[, xCol]), max(get_data(object)[, xCol]))
      ggplot2::ggplot(plot_data, ggplot2::aes_(x = plot_data[,1], y = ~value)) +
        ggplot2::geom_line(ggplot2::aes(color = factor(stress)), n = n) +
        ggplot2::labs(x = "" , y = paste("quantiles of", x_name, sep = " ")) +
        ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
    } else {
      return(plot_data)
    }
  }
}

# helper
.inverse <- function(f, lower = -100, upper = 100){
  return(function(y){
    stats::uniroot((function(x){f(x) - y}), lower = lower, upper = upper, extendInt = 'yes')$root
    })
}
