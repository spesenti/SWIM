#' Plotting Sensitivities of a Stressed Model
#'
#' Plots the sensitivity measures for components (random variables)
#'     of a stochastic model under the scenario weights.
#'
#' @inheritParams sensitivity
#' @param displ   Logical, if \code{TRUE} the plot is displayed,
#'                otherwise the data.frame for customised plotting with
#'                \code{ggplot} is returned (\code{default = TRUE}).
#'
#' @details For the definition of the sensitivity
#'     measures (\code{type}), see \code{\link{sensitivity}}.
#'
#'     Note that the Kolmogorov distance is the same for all inputs under
#'     the same stress for a SWIM object. Thus, it should only be used to 
#'     compare different stresses, not individual components.
#'
#' @return If \code{displ = TRUE}, a plot displaying the sensitivity
#'     measures of the stochastic model under the scenario weights.
#'     If \code{displ = FALSE}, a data.frame for customised plotting with
#'     \code{ggplot}. The data.frame
#'     contains the columns: \code{stress} (the stresses), \code{type}
#'     (the types of sensitivity), \code{X_all} (the random variables),
#'     \code{value} (the values of the sensitivities). \cr
#'     Denote by \code{result} the return of the function call, then
#'     \code{ggplot} can be called via:
#'     \deqn{ggplot(result, aes(x = X_{all}, y = value))}
#'     \deqn{ + geom_{point}(aes(color = factor(stress), shape = type)).}
#'
#' @examples
#' ## Consider the portfolio Y = X1 + X2 + X3 + X4 + X5,
#' ## where (X1, X2, X3, X4, X5) are correlated normally
#' ## distributed with equal mean and different standard deviations,
#' ## see the README for further details.
#'
#' \donttest{
#' set.seed(0)
#' SD <- c(70, 45, 50, 60, 75)
#' Corr <- matrix(rep(0.5, 5 ^ 2), nrow = 5) + diag(rep(1 - 0.5, 5))
#' if (!requireNamespace("mvtnorm", quietly = TRUE))
#'    stop("Package \"mvtnorm\" needed for this function
#'    to work. Please install it.")
#' x <- mvtnorm::rmvnorm(10 ^ 5,
#'    mean =  rep(100, 5),
#'    sigma = (SD %*% t(SD)) * Corr)
#' data <- data.frame(rowSums(x), x)
#' names(data) <- c("Y", "X1", "X2", "X3", "X4", "X5")
#' rev.stress <- stress(type = "VaR", x = data,
#'    alpha = c(0.75, 0.9), q_ratio = 1.1, k = 1)
#'
#' sensitivity(rev.stress, type = "all")
#' plot_sensitivity(rev.stress, xCol = 2:6, type = "Gamma")
#' plot_sensitivity(rev.stress, xCol = 6, wCol = 1, type = "all")
#' }
#'
#' @seealso See \code{\link{sensitivity}} for the values of the
#'     sensitivity measures of a stressed model and
#'     \code{\link{importance_rank}} for ranking of random
#'     variables according to their sensitivities.
#'
#' @author Silvana M. Pesenti
#'
#' @export

  plot_sensitivity <- function(object, xCol = "all", wCol = "all", 
                               type = c("Gamma", "Kolmogorov", "Wasserstein", "reverse", "all"),
                               f = NULL, k = NULL, s= NULL, displ = TRUE, p = 1){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw.")
  if (anyNA(object$x)) warning("x contains NA")
  if (missing(type)) type <- "all"
  if (!is.null(s)){
     if (!is.function(s)) stop("s must be a function")
  }
  if ((type == 'reverse' | type == 'all') && is.null(s)){
     warning("No s passed in. Using Gamma sensitivity instead.")
     s <- function(x) x
  }
   sens <- sensitivity(object, xCol = xCol, wCol = wCol, type = type, f, k, s=s, p)
   
   sens <- reshape2::melt(sens, id.var = c("stress", "type"), variable.name = "X_all")
   if (displ == TRUE){
     ggplot2::ggplot(sens, ggplot2::aes_(x = ~X_all, y = ~value)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(stress), shape = type)) +
      ggplot2::labs(x = "", y = "sensitivity") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 10))
   } else {
    return(sens)
   }
  }
