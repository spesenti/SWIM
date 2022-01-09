#' Standard Deviation and Variance of a Stressed Model
#' 
#' Provides the standard deviation and variance of stressed 
#'     model components (random variables) under the scenario weights. 
#' 
#' @inheritParams mean_stressed
#' 
#' @return \code{sd_stressed}: Return the standard deviation of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#'     The quantity can be evaluated at a vector. 
#' 
#' @details \code{sd_stressed}: The standard deviation of 
#'      a chosen model component, subject to the calculated scenario weights.
#'      
#' @describeIn sd_stressed Sample standard deviation of model components
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed standard deviation
#' sd_stressed(res1, xCol = "all", wCol = "all", base = TRUE)
#' 
#' ## stressed variance
#' var_stressed(res1, xCol = "all", wCol = "all", base = TRUE)
#' 
#' @author Kent Wu
#' 
#' @seealso See \code{\link{mean_stressed}} for means of stressed model components,
#' and \code{\link{cor_stressed}} for correlations between stressed model components. 
#' 
#' @export

sd_stressed <- function(object, xCol = "all", wCol = "all", base=FALSE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class 'SWIM' or 'SWIMw'")
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- as.matrix(get_weights(object)[ ,wCol])
  if (anyNA(x_data)) warning("x contains NA")
  
  mean_w <- mean_stressed(object, xCol, wCol, base)
  cname <- colnames(mean_w)
  rname <- rownames(mean_w)
  
  if (is.SWIM(object)){
    # K-L Divergence
    if (base == TRUE){
      old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
      new_weights <- cbind(old_weights, new_weights)
    }
    
    n <- dim(x_data)[1] # number of observations
    m <- dim(mean_w)[1] # number of weights
    d <- dim(mean_w)[2] # number of random variables
    
    temp <- do.call(rbind, lapply(1:m, function(i) (x_data - rep(mean_w[i,], each=n))^2))
    dim(new_weights) <- c(n, m, 1)
    dim(temp) <- c(n, m, d)
    sd <- do.call(rbind, lapply(1:m, function(i) sqrt(t(new_weights[,i,]) %*% temp[,i,] /(n-1) )))

  } else {
    # Wasserstein Distance
    u <- object$u
    sd <- c()
    
    for (c in 1:length(xCol)){
      temp <- c()
      for (i in 1:length(wCol)){
        index <- names(object$specs)[wCol[i]]
        k <- object$specs[[index]]$k
        if(is.character(k)) k_name <- k
        if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
        else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
    
        w <- get_weights(object)[ , wCol[i]]
        h <- object$h[[wCol[i]]](x_data[, c])
        
        lower_bracket = min(x_data[, c])#-(max(x_data[, c])-min(x_data[, c]))*0.1
        upper_bracket = max(x_data[, c])#+(max(x_data[, c])-min(x_data[, c]))*0.1
        
        if(k_name == colnames(get_data(object))[c]){
          # Get stressed quantile
          G.inv.fn <- Vectorize(object$str_FY_inv[[wCol[i]]])
        } else{
          # Get KDE
          G.fn <- function(x){
            return(sum(w * stats::pnorm((x - x_data[,c])/h)/length(x_data[,c])))
          }
          G.fn <- Vectorize(G.fn)
          G.inv.fn <- Vectorize(.inverse(G.fn, lower_bracket, upper_bracket))
        }
        
        # achieved mean and sd
        mean_achieved <- .integrate(G.inv.fn(u), u)
        sd_achieved <- sqrt(.integrate((G.inv.fn(u) - mean_achieved)^2, u))
        temp <- c(temp, sd_achieved)
      }
      
      if (base == TRUE){
        # Get KDE
        F.fn <- function(x){
          return(sum(stats::pnorm((x - x_data[, c])/h)/length(x_data[, c])))
        }
        F.fn <- Vectorize(F.fn)
        F.inv.fn <- Vectorize(.inverse(F.fn, lower_bracket, upper_bracket))
        
        mean_achieved <- .integrate(F.inv.fn(u), u)
        sd_achieved <- sqrt(.integrate((F.inv.fn(u) - mean_achieved)^2, u))
        temp <- c(sd_achieved, temp)
      }
      
      sd <- cbind(sd, temp)
      }
    }
  
  colnames(sd) <- cname
  rownames(sd) <- rname
  
  return(sd)
  }

#' @describeIn sd_stressed Sample variance of model components
#' 
#' @return \code{var_stressed}: Return the variance of the \code{xCol}
#'     component of the stressed model with weights \code{wCol}.
#'     The quantity can be evaluated at a vector. 
#'     
#' @details \code{var_stressed}: The variance of 
#'      a chosen stressed model component, subject to the calculated scenario weights.
#'
#' @export

var_stressed <- function(object, xCol = "all", wCol = "all", base=FALSE){
  var <- (sd_stressed(object, xCol, wCol, base))^2
  return(var)
}
