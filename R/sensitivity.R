#' Sensitivities of a Stressed Model
#'
#' Provides different sensitivity measures that compare the stressed
#'     and the baseline model.
#'
#' @inheritParams summary.SWIM
#' @inheritParams stress_moment
#' @param f       A function, or list of functions, that, applied to
#'                \code{x}, constitute the transformation of the data
#'                for which the sensitivity is calculated.
#' @param type    Character, one of \code{"Gamma", "Kolmogorov",
#'                "Wasserstein", "reverse", "all"} (\code{default = "all"}).
#' @param s       A function that, applied to \code{x}, defines the reverse
#'                sensitivity measure. If \code{type = "reverse"} and 
#'                \code{s = NULL}, defaults to \code{type = "Gamma"}.
#' @param xCol    Numeric or character vector, (names of) the columns
#'                of the underlying data of the \code{object}
#'                (\code{default = "all"}). If \code{xCol = NULL}, only
#'                the transformed data \code{f(x)} is considered.
#' @param p       Numeric vector, the p-th moment of Wasserstein distance (\code{default = 1}). 
#'
#' @details Provides sensitivity measures that compare the stressed and
#'     the baseline model. Implemented sensitivity measures:
#'     \enumerate{
#'     \item
#'       \code{Gamma}, the \emph{Reverse Sensitivity Measure}, defined
#'       for a random variable \code{Y} and scenario weights \code{w} by
#'       \deqn{Gamma = ( E(Y * w) - E(Y) ) / c,}
#'       where \code{c} is a normalisation constant such that
#'       \code{|Gamma| <= 1}, see
#'       \insertCite{Pesenti2019reverse}{SWIM}. Loosely speaking, the
#'       Reverse Sensitivity Measure is the normalised difference
#'       between the first moment of the stressed and the baseline
#'       distributions of \code{Y}.
#'
#'     \item
#'       \code{Kolmogorov}, the Kolmogorov distance, defined for
#'       distribution functions \code{F,G} by
#'       \deqn{Kolmogorov = sup |F(x) - G(x)|.}
#'
#'     \item
#'       \code{Wasserstein}, the Wasserstein distance of order 1, defined
#'       for two distribution functions \code{F,G} by
#'       \deqn{Wasserstein = \int |F(x) - G(x)| dx.}
#'     
#'     \item
#'       \code{reverse}, the \emph{General Reverse Sensitivity Measure}, defined
#'       for a random variable \code{Y}, scenario weights \code{w}, and a function
#'       \code{s:R -> R} by \deqn{epsilon = ( E(s(Y) * w) - E(s(Y)) ) / c,}
#'       where \code{c} is a normalisation constant such that
#'       \code{|epsilon| <= 1}. \code{Gamma} is a special instance of
#'       the reverse sensitivity measure when \code{s} is the identity function.
#'     }
#'     
#'     If \code{f} and \code{k} are provided, the sensitivity of the
#'     transformed data is returned.
#'
#' @return A data.frame containing the sensitivity measures of the
#'     stressed model with rows corresponding to different random
#'     variables. The first two rows specify the \code{stress} and
#'     \code{type} of the sensitivity measure.
#'
#' @examples
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "log-normal" = rlnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x,
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#'
#' sensitivity(res1, wCol = 1, type = "all")
#' ## sensitivity of log-transformed data
#' sensitivity(res1, wCol = 1, type = "all",
#'   f = list(function(x)log(x), function(x)log(x)), k = list(1,2))
#'
#' ## Consider the portfolio Y = X1 + X2 + X3 + X4 + X5,
#' ## where (X1, X2, X3, X4, X5) are correlated normally
#' ## distributed with equal mean and different standard deviations,
#' ## see the README for further details.
#'
#'
#' \dontrun{
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
#' ## sensitivity to sub-portfolios X1 + X2 and X3 + X4
#' sensitivity(rev.stress, xCol = NULL, type = "Gamma",
#'   f = rep(list(function(x)x[1] + x[2]), 2), k = list(c(2, 3), c(4, 5)))
#' plot_sensitivity(rev.stress, xCol = 2:6, type = "Gamma")
#' importance_rank(rev.stress, xCol = 2:6, type = "Gamma")
#' }
#' 
#' @author Silvana M. Pesenti, Zhuomin Mao
#'
#' @seealso See \code{\link{importance_rank}} for ranking of random
#'     variables according to their sensitivities,
#'     \code{\link{plot_sensitivity}} for plotting
#'     sensitivity measures and \code{\link{summary}} for
#'     summary statistics of a stressed model.
#'
#' @references \insertRef{Pesenti2019reverse}{SWIM}
#'
#' @export
#'

  sensitivity <- function(object, xCol = "all", wCol = "all",
                          type = c("Gamma", "Kolmogorov", "Wasserstein", "reverse", "all"),
                          f = NULL, k = NULL, s = NULL, p = 1){
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
   if (anyNA(object$x)) warning("x contains NA")
   if (missing(type)) type <- "all"
   if (!is.null(f) | !is.null(k)){
   if (is.function(f)) f <- list(f)
   if (!all(sapply(f, is.function))) stop("f must be a list of functions")
   if (is.numeric(k)) k <- list(k)
   if (!all(sapply(k, is.numeric))) stop("k must be a list of numeric vectors")
   if (length(f) != length(k)) stop("Objects f and k must have the same length.")
   }
   if (!is.null(s)){
     if (!is.function(s)) stop("s must be a function")
   }
   if ((type == 'reverse' | type == 'all') && is.null(s)){
     warning("No s passed in. Using Gamma sensitivity instead.")
     s <- function(x) x
   }
   if (!is.null(xCol)){
   if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
   if (is.character(xCol) && xCol != "all") cname <- xCol
   if (is.null(colnames(get_data(object)))){
    cname <-  paste("X", as.character(xCol), sep = "")
   } else if (!is.character(xCol)){
    cname <- colnames(get_data(object))[xCol]
   }
   x_data <- get_data(object)[ , xCol]
   }
   if (!is.null(f)){
      z <- matrix(0, ncol = length(f), nrow = nrow(get_data(object)))
      for (i in 1:length(f)){
         z[, i] <- apply(get_data(object)[, k[[i]], drop = FALSE], 1, f[[i]])
      }
      if(is.null(xCol)) cname <- NULL
      cname <- c(cname, paste("f", 1:length(f), sep = ""))
      if(is.null(xCol)) x_data <- NULL
      x_data <- cbind(x_data, z)
      colnames(x_data) <- cname
   }

   if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
   new_weights <- get_weights(object)[ , wCol]
   sens_w <- stats::setNames(data.frame(matrix(ncol = length(x_data) + 2, nrow = 0)), c("stress", "type", cname))
   
   if (type == "Gamma" || type == "all"){
    sens_gamma_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .gamma, z = z)
    sens_gw <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_gamma_w)
    if (length(wCol) == 1) sens_gw <- as.matrix(t(sens_gw))
    if (length(xCol) == 1) colnames(sens_gw) <- cname
    sens_w <- rbind(sens_w, data.frame(stress = names(object$specs)[wCol], type = rep("Gamma", length.out = length(wCol)), sens_gw))
   }

   if (type == "Kolmogorov" || type == "all"){
    sens_kolmogorov_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .kolmogorov, z = z)
    sens_kw <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_kolmogorov_w)
    if (length(wCol) == 1) sens_kw <- as.matrix(t(sens_kw))
    if (length(xCol) == 1) colnames(sens_kw) <- cname
    sens_w <- rbind(sens_w, data.frame(stress = names(object$specs)[wCol], type = rep("Kolmogorov", length.out = length(wCol)), sens_kw))
   }

   if (type == "Wasserstein" || type == "all"){
    for (p_value in c(p)) {
      sens_wasser_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .wasserstein, z = z, p = p_value)
      sens_ww <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_wasser_w)
      if (length(wCol) == 1) sens_ww <- as.matrix(t(sens_ww))
      if (length(xCol) == 1) colnames(sens_ww) <- cname
      sens_w <- rbind(sens_w, data.frame(stress = names(object$specs)[wCol], type = rep("Wasserstein", length.out = length(wCol)), sens_ww))
     
      # Paste p to Wasserstein
      idx <- sens_w["type"] == "Wasserstein"
      sens_w[idx, "type"] <- paste("Wasserstein", "p =", p_value) 
      }
   }
   
   if (type == "reverse" || type == "all"){
     sens_reverse_w <- function(z) apply(X = as.matrix(new_weights), MARGIN = 2, FUN = .reverse, z = z, s=s)
     sens_rw <- apply(X = as.matrix(x_data), MARGIN = 2, FUN = sens_reverse_w)
     if (length(wCol) == 1) sens_rw <- as.matrix(t(sens_rw))
     if (length(xCol) == 1) colnames(sens_rw) <- cname
     sens_w <- rbind(sens_w, data.frame(stress = names(object$specs)[wCol], type = rep("Reverse", length.out = length(wCol)), sens_rw))
   }
   
   rownames(sens_w) <- NULL
   return(sens_w)
  }



 # help function Reverse Sensitivity, Gamma
 # comparison between input vectors for a given stress
  .gamma <- function(z, w){
   w <- as.numeric(w)
   w_comm <- sort(w)[rank(z, ties.method = "first")]
   w_counter <- sort(w, decreasing = TRUE)[rank(z, ties.method = "first")]
   if (stats::cov(z, w) >= 0){
    gamma_sens <- stats::cov(z, w) / stats::cov(z, w_comm)
   } else {
    gamma_sens <- - stats::cov(z, w) / stats::cov(z, w_counter)
   }
   return(gamma_sens)
  }

 # help function Kolmogorov distance
 # maximal difference between the corresponding ecdf
 # comparison between different stresses. All inputs from one
 # stress have the same Kolmogorov distance.
  .kolmogorov <- function(z, w){
    n <- length(z)
    # print(length(z))
    # print(length(w))
    # print(n)
    xw_cdf <- cumsum(w[order(z)])[1:(n-1)]
    kol_sense <- max(abs(xw_cdf - 1:(n-1))) / n
    return(kol_sense)
  }

 # help function Wasserstein distance of order p = 1
 # x   vector
 # w   vector of weights

  .wasserstein <- function(z, w, p = 1){
    n <- length(z)
    x_sort <- sort(z)
    w_cdf <- cumsum(w[order(z)])[1:(n - 1)]
    x_diff <- diff(x_sort, lag = 1)
    wasser_sens <- (sum(abs(w_cdf - 1:(n-1))^(p) * x_diff) / n)^(1/p)
    return(wasser_sens)
  }
  
  # help function Reverse Sensitivity
  # comparison between input vectors for a given stress and function s
  .reverse <- function(z, s, w){
    w <- as.numeric(w)
    
    EQ_sX <- mean(sapply(z, s) * w)
    EP_sX <- mean(sapply(z, s))
    
    z_inc <- sort(z)
    w_inc <- sort(w)
    w_dec <- sort(w, decreasing = TRUE)

    if (EQ_sX >= EP_sX){
      max_EQ <- mean(sapply(z_inc, s) * w_inc)
      reverse_sens <- (EQ_sX - EP_sX) / (max_EQ - EP_sX)
    } else {
      min_EQ <- mean(sapply(z_inc, s) * w_dec)
      reverse_sens <- - (EQ_sX - EP_sX) / (min_EQ - EP_sX)
    }
    return(reverse_sens)
  }
