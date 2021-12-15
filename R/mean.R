#' Mean of a Stressed Model
#' 
#' Provides the mean of stressed model components (random variables) under the scenario weights. 
#' 
#' @inheritParams summary.SWIM 
#' 
#' @return A matrix containing the means of the \code{xCol}
#'     components of the stressed model with weights \code{wCol}.
#' 
#' @details \code{mean_stressed}: Sample mean of chosen stressed model components, subject to the calculated scenario weights.
#'
#' 
#' @examples      
#' ## example with a stress on VaR
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "VaR", x = x, 
#'   alpha = c(0.9, 0.95), q_ratio = 1.05)
#' ## stressed mean
#' mean_stressed(res1, xCol = "all", wCol = "all", base = TRUE)
#' 
#' @author Kent Wu
#' 
#' @seealso See \code{\link{var_stressed}} and \code{\link{sd_stressed}} compute
#'     stressed variance and standard deviations under the scenario weights, respectively.
#'     
#' @export

mean_stressed <- function(object, xCol = "all", wCol = "all", base=FALSE){
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class 'SWIM' or 'SWIMw'")
  if (anyNA(object$x)) warning("x contains NA")
  
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  cname <- colnames(x_data)
  if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
  new_weights <- as.matrix(get_weights(object)[ ,wCol])  
  
  if (is.SWIM(object)){
    # K-L Divergence
    n <- dim(x_data)[1]
    mean <- t(new_weights) %*% x_data / n
    colnames(mean) <- cname
    rownames(mean) <- names(object$specs)[wCol]
    
    if (base == TRUE){
      old_weights <- matrix(rep(1, length(x_data[,1])), ncol = 1)
      mean_base <- t(old_weights) %*% x_data / n
      mean <- rbind(mean_base, mean)
      rownames(mean) <- c("base", names(object$specs)[wCol])
    }
    
  } else {
    # Wasserstein Distance
    u <- object$u
    mean <- c()
    
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
        temp <- c(temp, mean_achieved)
      }
      
      if (base == TRUE){
        # Get KDE
        F.fn <- function(x){
          return(sum(stats::pnorm((x - x_data[, c])/h)/length(x_data[, c])))
        }
        F.fn <- Vectorize(F.fn)
        F.inv.fn <- Vectorize(.inverse(F.fn, lower_bracket, upper_bracket))
        
        mean_achieved <- .integrate(F.inv.fn(u), u)
        temp <- c(mean_achieved, temp)
      }
      mean <- cbind(mean, temp)
      
    }
    colnames(mean) <- cname
    if (base == TRUE) rownames(mean) <- c("base", names(object$specs)[wCol]) else rownames(mean) <- names(object$specs)[wCol]
  }
  
  return(mean)
  }

