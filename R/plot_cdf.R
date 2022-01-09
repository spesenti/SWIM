#' Plotting the Distribution Functions of a Stressed Model
#'
#' Plots the empirical distribution function of a stressed SWIM model
#'     component (random variable) or KDE distribution function of a stressed
#'     SWIMw model component under the scenario weights.
#'
#' @inheritParams  sensitivity
#' @inheritParams  plot_sensitivity
#' @inheritParams  summary.SWIM
#' @param xCol     Numeric or character, (name of) the column of the underlying data
#'                 of the \code{object} (\code{default = 1}).
#' @param n        Integer, the number of points used to plot
#'                 \code{stat_ecdf} in \code{ggplot} (\code{default
#'                 = 500}).
#' @param x_limits Vector, the limits of the x-axis of the plot, the
#'                 value for \code{xlim} in the \code{coord_cartesian}
#'                 function in \code{ggplot}.
#' @param y_limits Vector, the limits of the y-axis of the plot, the
#'                 value for \code{ylim} in the \code{coord_cartesian}
#'                 function in \code{ggplot}.
#
#' @return If \code{displ = TRUE}, a plot displaying the empirical or KDE
#'     distribution function of the stochastic model under the
#'     scenario weights.
#'
#'     If \code{displ = FALSE}, a data.frame for customised plotting with
#'     \code{ggplot}. The data.frame contains the columns: the column,
#'     \code{xCol}, of the data of the stressed model,
#'     \code{stress} (the stresses) and \code{value} (the values). \cr
#'     Denote by \code{res} the return of the function call, then
#'     \code{ggplot} can be called via:
#'     \deqn{ggplot(res, aes(x = res[ ,1], w = value))}
#'     \deqn{ + stat_{ecdf}(aes(color = factor(stress)), n = n).}
#'     Note that the ggplot2 default of \code{stat_ecdf} does not
#'     take \code{weight} as an aesthetic. We use the workaround
#'     by Nicolas Woloszko, see Note below.
#'
#' @note This function is based on the ggplot \code{stat_ecdf}
#'     function. However, the \code{stat_ecdf} does not allow for
#'     specifying \code{weights}, thus the function is based on
#'     the workaround by Nicolas Woloszko, see
#'     \url{https://github.com/NicolasWoloszko/stat_ecdf_weighted}.
#'
#' @examples
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(10 ^ 5),
#'   "gamma" = rgamma(10 ^ 5, shape = 2)))
#' res1 <- stress(type = "VaR", x = x,
#'   alpha = c(0.75, 0.95), q_ratio = 1.15)
#' plot_cdf(res1, xCol = 1, wCol = 1:2, base = TRUE)
#' plot_cdf(res1, xCol = 1, wCol = 1:2, base = TRUE,
#'   x_limits = c(0, 5), y_limits = c(0.5, 1))
#'
#' @author Silvana M. Pesenti, Zhuomin Mao
#'
#' @seealso See \code{\link{cdf}} for the empirical or KDE distribution function
#'     of a stressed model and \code{\link{quantile_stressed}} for
#'     sample quantiles of a stressed model.
#'
#' @export

  plot_cdf <- function(object, xCol = 1, wCol = "all", base = FALSE, n = 500,                            
                       x_limits, y_limits, displ = TRUE){
   value <- NULL
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
   if (anyNA(object$x)) warning("x contains NA")
   x_data <- get_data(object)[, xCol]
   if(is.character(xCol)) x_name <- xCol
   if(is.null(colnames(get_data(object)))) x_name <- paste("X", xCol, sep = "") else if(!is.character(xCol)) x_name <- colnames(get_data(object))[xCol]
   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
   
   if (is.SWIM(object) ){
      # K-L Divergence
      plot_data <- data.frame(x_data, get_weights(object)[ , wCol])
      
      # Display components' names
      names(plot_data) <- c(x_name, names(object$specs)[wCol])
      
      if (base == TRUE){
         plot_data <- cbind(plot_data, base = rep(1, length(x_data)))
      }
      plot_data <- reshape2::melt(plot_data, id.var = x_name, variable.name = "stress", value.name = "value")
      
      if (displ == TRUE){
         if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data))
         if (missing(y_limits)) y_limits <- c(0,1)
         ggplot2::ggplot(plot_data, ggplot2::aes_(x = plot_data[,1], weight = ~value)) +
            stat_ecdf(ggplot2::aes(color = factor(stress)), n = n) +
            ggplot2::labs(x = x_name, y = "ecdf") +
            ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
      } else {
         return(plot_data)
      }
   } else {
      # Wasserstein Distance
      plot.data <- data.frame(row.names = 1:length(x_data))
      for (i in 1:length(wCol)) {
         w <- get_weights(object)[ , i]
         h <- object$h[[i]](x_data)
         
         index <- names(object$specs)[i]
         k <- object$specs[[index]]$k
         if(is.character(k)) k_name <- k
         if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
         else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
         
         if(k_name == x_name){
            # Get stressed distribution
            G.fn <- object$str_FY[[i]]
         } else{
            # Get KDE
            G.fn <- function(x){
               return(sum(w * stats::pnorm((x - x_data)/h)/length(x_data)))
            }
            G.fn <- Vectorize(G.fn)
         }
         
         # Display components' names
         curr.data <- data.frame(x_data, G.fn(x_data))
         names(curr.data) <- c(x_name, names(object$specs)[i])
         plot.data <- cbind(plot.data, curr.data)
         
      }
      
      if (base == TRUE){
         # Get KDE
         F.fn <- function(x){
            return(sum(stats::pnorm((x - x_data)/h)/length(x_data)))
         }
         F.fn <- Vectorize(F.fn)
         plot.data <- cbind(plot.data, base = F.fn(x_data))
      }
      
      plot.data <- reshape2::melt(plot.data, id.var = x_name, variable.name = "stress", value.name = "value")
      if (displ == TRUE){
         if (missing(x_limits)) x_limits <- c(min(x_data)-0.1, max(x_data)+0.1)
         if (missing(y_limits)) y_limits <- c(0,1)
         ggplot2::ggplot(plot.data, ggplot2::aes(x = plot.data[,1], value, col=stress)) +
            ggplot2::geom_line(ggplot2::aes(color = stress)) +
            ggplot2::labs(x = x_name, y = "cdf") +
            ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.title = ggplot2::element_blank(), 
                           legend.key = ggplot2::element_blank(), 
                           legend.text = ggplot2::element_text(size = 10))
      } else {
         return(plot.data)
      }
   }
  }
