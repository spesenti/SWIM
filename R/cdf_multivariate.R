#' Empirical Distribution Function of a Stressed Model
#' 
#' Provides the empirical distribution of a stressed model component (random variable) under the scenario weights
#' 
#' @inheritParams sensitivity 
#' @param xCol    Numeric, (name of) the column of the underlying data 
#'                of the \code{object} (\code{default = 1}). 
#' @param wCol    Vector or characters, the columns of the scenario weights 
#'                of the \code{object} (\code{default = "all"}).
#' @param grid    Vector, the empirical distribution of the \code{xCol} component
#'                of the stressed model with weights \code{wCol}. 
#' @param base    Logical, if TRUE, statistics under the baseline are also returned (default = "FALSE").]
#' 
#' @details The \code{cdf_stressed} returns the values of the empirical distribution function 
#'          of the \code{xCol} model components for weights \code{wCol}. In contrast, the \code{cdf} function 
#'          returns a function, analogous to the \code{ecdf} from the base package. The 
#'          function \code{cdf_stressed} is the \code{cdf} function applied to grid.
#' 
#' @return A matrix containing the empirical distribution function 
#'         applied to \code{grid} of the \code{xCol} components of the 
#'         stressed model with weights \code{wCol}.
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' grid <- seq(min(x$normal), max(x$normal), length.out = 5)
#' ## stressed empirical distribution function
#' cdf_stressed(res1, xCol = 1, wCol = "all", grid = grid)
#' 
#' @author Kent Wu
#' 
#' @seealso See \code{\link{cdf}} for the empirical distribution function of a stressed 
#'     model component and \code{\link{quantile_stressed}} for sample quantiles of 
#'     a stressed model. 
#' @export
#' 

cdf_stressed <- function(object, xCol = 1, wCol = "all", grid, base=FALSE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
  if (anyNA(object$x)) warning("x contains NA")
  
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- get_weights(object)[ , wCol]

  if (length(xCol) > 1 || xCol == "all") stop("Input xCol has dimension larger than 1")
  x_data <- as.matrix(get_data(object, xCol = xCol))

  # loop over the selected weights
  res <- matrix(NA, ncol=length(grid), nrow=length(wCol))
  for (i in 1:length(wCol)) {res[i, ] <- cdf(object, xCol = xCol, wCol = wCol[i])(grid)}
  
  rownames(res) <- names(object$specs)[wCol]
  
  if (base == TRUE){
    base <- (stats::ecdf(x_data[, xCol])(grid))
    res <- rbind(res, base)
  }
  
  colnames(res) <-round(grid, 4)
  return(res)
}


