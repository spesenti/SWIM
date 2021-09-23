#' Value-at-Risk and Expected Shortfall of a Stressed Model
#'
#' Provides the Value-at-Risk (VaR) and the Expected Shortfall (ES)
#'     for components (random variables) of a stochastic model.
#'
#' @inheritParams summary.SWIM
#' @param alpha   Numeric vector, the levels of the stressed VaR and ES
#'                (\code{default = 0.95}).
#' @param wCol    Numeric, the column of the scenario weights
#'                of the \code{object} (\code{default = 1}).
#' @param gamma Function that defines the gamma of the risk measure. If null,
#' the Expected Shortfall (ES) will be used.\cr
#'
#' @return \code{VaR_stressed}: Returns a matrix with the empirical or KDE VaR's
#'     at level \code{alpha} of
#'     model components specified in \code{xCol}, under the scenario weights
#'     \code{wCol}.
#'
#' @details \code{VaR_stressed}: The VaR of a model is the VaR (quantile) of
#'      a chosen stressed model component, subject to the calculated scenario weights.
#'      The VaR at level \code{alpha} of a stressed model component with
#'      stressed distribution function F^W is defined as its
#'      left-quantile at alpha:
#'      \deqn{VaR_{alpha}^W = F^{W,-1}(alpha).}
#'
#'      The function \code{VaR_stressed} provides the empirical quantile, whereas
#'      the function \code{quantile_stressed} calculates quantiles of model
#'      components with different interpolations.
#'
#' @author Silvana M. Pesenti, Zhuomin Mao
#' @describeIn VaR_stressed Value-at-Risk of a stressed model.
#'
#' @seealso See \code{quantile_stressed} for quantiles other than the
#'     empirical quantiles and \code{cdf} for the empirical or KDE distribution
#'     function of a stressed model.
#'
#' @examples
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x,
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed ES
#' quantile_stressed(res1, probs = seq(0.9, 0.99, 0.01),
#'                     xCol = 1, wCol = 2, type = "i/n")
#' quantile(x[, 1],  probs = seq(0.9, 0.99, 0.01), type = 1)
#' VaR_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = 1,
#'                     wCol = 2, base = TRUE)
#'
#' ## the ES of both model components under stress one
#' ES_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = "all",
#'                     wCol = 1)
#' ## the ES of both model components under stress two
#' ES_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = "all",
#'                     wCol = 2)
#'
#' @export

  VaR_stressed <- function(object, alpha = 0.95, xCol = "all", wCol = 1, base = FALSE) {
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
   if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")

   if (is.SWIM(object)){
      # K-L Divergence
      VaR <- quantile_stressed(object, probs = alpha, xCol = xCol, wCol = wCol, type = "i/n")
      if (base == TRUE) {
         VaR_base <- as.matrix(apply(X = as.matrix(get_data(object = object, xCol = xCol)), MARGIN = 2,
                                     FUN = stats::quantile, probs = alpha, type= 1))
         colnames(VaR_base) <- paste("base", colnames(get_data(object = object, xCol = xCol)))
         VaR <- cbind(VaR, VaR_base)
      } 
   } else {
      # Wasserstein Distance
      VaR <- quantile_stressed(object, probs = alpha, xCol = xCol, wCol = wCol, type = "quantile", base=base)
   }

   return(VaR)
}


